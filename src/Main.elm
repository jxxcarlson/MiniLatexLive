module Main exposing (main)

import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import MiniLatex
import MiniLatex.Edit exposing (Data)
import MiniLatex.Render exposing(MathJaxRenderOption(..))
import Random
import StringsV1
import StringsV2
import Style exposing (..)
import Task
import Buffer exposing (Buffer)
import Editor exposing (EditorConfig, PEEditorMsg, State)
import Editor.Config exposing (WrapOption(..))
import SingleSlider as Slider


initialText =
    StringsV2.initialText


main : Program Flags (Model (Html Msg)) Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model a =
    { sourceText : String
    , renderedText : a
    , macroText : String
    , editRecord : Data a
    , debounce : Debounce String
    , counter : Int
    , seed : Int
    , editorBuffer : Buffer
    , editorState : State
    }


type Msg
    = Clear
    | Render String
    | GetContent String
    | GetMacroText String
    | DebounceMsg Debounce.Msg
    | GenerateSeed
    | NewSeed Int
    | FullRender
    | RestoreText
    | ExampleText
    | EditorMsg PEEditorMsg
    | SliderMsg Slider.Msg


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 250
    , transform = DebounceMsg
    }


type alias Flags =
    {}


init : Flags -> ( Model (Html msg), Cmd Msg )
init flags =
    let
        editRecord =
            MiniLatex.Edit.init Delay 0 initialText

        model =
            { sourceText = initialText
            , macroText = initialMacroText
            , renderedText = render Delay (prependMacros initialMacroText initialText)
            , editRecord = editRecord
            , debounce = Debounce.init
            , counter = 0
            , seed = 0
            , editorBuffer = Buffer.init initialText
            , editorState = Editor.init editorConfig
            }
    in
    ( model, Cmd.none )

editorConfig =
    { editorMsg = EditorMsg
    , sliderMsg = SliderMsg
    , editorStyle = editorStyle
    , width = 600
    , lines = 30
    , showInfoPanel = True
    , wrapParams = { maximumWidth = 65, optimalWidth = 60, stringWidth = String.length }
    , wrapOption = DontWrap
    }

editorStyle : List (Html.Attribute msg)
editorStyle =
    [ HA.style "background-color" "#dd0000"
    , HA.style "border" "solid 0.5px"
    ]

initialMacroText =
    normalize StringsV1.macros


subscriptions : Model (Html msg) -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SliderMsg <|
            Slider.subscriptions (Editor.slider model.editorState)
        ]


update : Msg -> Model (Html msg) -> ( Model (Html msg), Cmd Msg )
update msg model =
    case msg of
        EditorMsg msg_ ->
            let
                ( editor_, content, cmd ) =
                    Editor.update model.editorBuffer msg_ model.editorState

                newSourceText = Buffer.toString content

                ( debounce, cmd2 ) =
                            Debounce.push debounceConfig newSourceText model.debounce

            in
            ( { model
                | editorState = editor_
                , editorBuffer = content
                , sourceText = newSourceText
                , debounce = debounce
              }
            , Cmd.batch [Cmd.map EditorMsg cmd, cmd2]
            )


        SliderMsg sliderMsg ->
          let
            (newEditorState, cmd) = Editor.sliderUpdate sliderMsg  model.editorState model.editorBuffer
          in
            ( { model | editorState = newEditorState }, cmd  |> Cmd.map SliderMsg )

        GetContent str ->
            processNewContent model str

        GetMacroText str ->
            ( { model | macroText = str }, Cmd.none )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast render_)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        Render str ->
            let
                n =
                    String.fromInt model.counter

                newEditRecord =
                    MiniLatex.Edit.update NoDelay model.seed (prependMacros model.macroText str) model.editRecord
            in
            ( { model
                | editRecord = newEditRecord
                , renderedText = renderFromEditRecord model.counter newEditRecord
                , counter = model.counter + 1
              }
            , Random.generate NewSeed (Random.int 1 10000)
            )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear ->
            let
                editRecord =
                    MiniLatex.Edit.init Delay 0 ""
            in
            ( { model
                | sourceText = ""
                , editRecord = editRecord
                , renderedText = renderFromEditRecord model.counter editRecord
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        FullRender ->
            let
                editRecord =
                    MiniLatex.Edit.init Delay model.seed (prependMacros model.macroText model.sourceText)
            in
            ( { model
                | counter = model.counter + 1
                , editRecord = editRecord
                , renderedText = renderFromEditRecord model.counter editRecord
              }
            , Cmd.none
            )

        RestoreText ->
            let
                editRecord =
                    MiniLatex.Edit.init Delay model.seed (prependMacros initialMacroText initialText)
            in
            ( { model
                | counter = model.counter + 1
                , editRecord = editRecord
                , sourceText = initialText
                , renderedText = renderFromEditRecord model.counter editRecord
              }
            , Cmd.none
            )

        ExampleText ->
            let
                editRecord =
                    MiniLatex.Edit.init Delay model.seed (prependMacros initialMacroText StringsV1.mathExampleText)
            in
            ( { model
                | counter = model.counter + 1
                , editRecord = editRecord
                , sourceText = StringsV1.mathExampleText
                , renderedText = renderFromEditRecord model.counter editRecord
              }
            , Cmd.none
            )

processNewContent model str =
    let
       ( debounce, cmd ) =
            Debounce.push debounceConfig str model.debounce
    in
    ( { model
        | sourceText = str
        , debounce = debounce
      }
    , cmd
    )

normalize : String -> String
normalize str =
    str |> String.lines |> List.filter (\x -> x /= "") |> String.join "\n"


prependMacros : String -> String -> String
prependMacros macros_ sourceText =
    "$$\n" ++ (macros_ |> normalize) ++ "\n$$\n\n" ++ sourceText


renderFromEditRecord : Int -> Data (Html msg) -> Html msg
renderFromEditRecord counter editRecord =
    let
        _ =
            Debug.log "(paragraphs, rendered)" ( List.length editRecord.paragraphs, List.length editRecord.renderedParagraphs )
    in
    MiniLatex.Edit.get editRecord
        |> List.map (\x -> Html.div [ HA.style "margin-bottom" "0.65em" ] [ x ])
        |> Html.div []


render_ : String -> Cmd Msg
render_ str =
    Task.perform Render (Task.succeed str)


render : MathJaxRenderOption -> String -> Html msg
render delay sourceText =
    let
        macroDefinitions =
            initialMacroText
    in
    MiniLatex.render delay macroDefinitions sourceText



--
-- VIEW FUNCTIONS
---


view : Model (Html Msg) -> Html Msg
view model =
    div (outerStyle ++ [ HA.class "container" ])
        [ lhs model
        , renderedSource model
        ]


lhs model =
    div [ HA.class "lhs", style "width" "600px" ]
        [ h1 [ style "margin-left" "20px" ] [ text "MiniLatex Demo" ]
        , label "Edit or write new LaTeX below. It will be rendered in real time."
        , Editor.embedded editorConfig model.editorState model.editorBuffer
        , p [ style "margin-left" "20px", style "font-style" "italic" ]
            [ text "For more information about MiniLaTeX, please go to  "
            , a [ href "https://minilatex.io", target "_blank" ] [ text "minilatex.io" ]
            ]
        ]



display : Model (Html Msg) -> Html Msg
display model =
    div []
        [ h1 [ style "margin-left" "20px" ] [ text "MiniLatex Demo" ]
        , label "Edit or write new LaTeX below. It will be rendered in real time."
        , editor model
        , div [] [ renderedSource model ]
        ]


label text_ =
    p labelStyle [ text text_ ]


editor : Model (Html msg) -> Html Msg
editor model =
    div []
        [ textarea (editorTextStyle ++ [ onInput GetContent, value model.sourceText ]) []
        , p [ style "clear" "left", style "margin-left" "20px", style "margin-top" "-20px" ] [ clearButton 60, restoreTextButton 80, fullRenderButton 100 ]
        ]


macroPanel : Model (Html msg) -> Html Msg
macroPanel model =
    Html.div []
        [ textarea (macroPanelStyle ++ [ onInput GetMacroText, value model.macroText ]) []
        , p [ style "clear" "left", style "margin-left" "20px", style "padding-top" "10px" ]
            [ text "Macros: write one macro per line (right panel)" ]
        ]


renderedSource : Model (Html msg) -> Html msg
renderedSource model =
    Html.div (renderedSourceStyle ++ [ HA.class "rhs" ])
        [ model.renderedText ]



--
-- BUTTONS
--


clearButton width =
    button ([ onClick Clear ] ++ buttonStyle colorBlue width) [ text "Clear" ]


fullRenderButton width =
    button ([ onClick FullRender ] ++ buttonStyle colorBlue width) [ text "Full Render" ]


restoreTextButton width =
    button ([ onClick RestoreText ] ++ buttonStyle colorBlue width) [ text "Restore" ]


exampleButton width =
    button ([ onClick ExampleText ] ++ buttonStyle colorBlue width) [ text "Example 2" ]
