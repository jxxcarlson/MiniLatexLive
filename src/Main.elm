module Main exposing (main)

import Browser
import Debounce exposing (Debounce)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode
import MiniLatex.Differ exposing (EditRecord)
import MiniLatex.MiniLatex as MiniLatex
import Random
import Task
import Strings
import Style exposing (..)


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
    , editRecord : EditRecord a
    , debounce : Debounce String
    , counter : Int
    , seed : Int
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
            MiniLatex.initializeEditRecord 0 Strings.initialText

        model =
            { sourceText = Strings.initialText
            , macroText = initialMacroText
            , renderedText = render (prependMacros initialMacroText Strings.initialText)
            , editRecord = editRecord
            , debounce = Debounce.init
            , counter = 0
            , seed = 0
            }
    in
        ( model, Cmd.none )


initialMacroText =
    normalize Strings.macros


subscriptions : Model (Html msg) -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model (Html msg) -> ( Model (Html msg), Cmd Msg )
update msg model =
    case msg of
        GetContent str ->
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
                    MiniLatex.updateEditRecord model.seed model.editRecord (prependMacros model.macroText str)
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
                    MiniLatex.initializeEditRecord 0 ""
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
                    MiniLatex.initializeEditRecord model.seed (prependMacros model.macroText model.sourceText)
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
                    MiniLatex.initializeEditRecord model.seed (prependMacros initialMacroText Strings.initialText)
            in
                ( { model
                    | counter = model.counter + 1
                    , editRecord = editRecord
                    , sourceText = Strings.initialText
                    , renderedText = renderFromEditRecord model.counter editRecord
                  }
                , Cmd.none
                )

        ExampleText ->
            let
                editRecord =
                    MiniLatex.initializeEditRecord model.seed (prependMacros initialMacroText Strings.mathExampleText)
            in
                ( { model
                    | counter = model.counter + 1
                    , editRecord = editRecord
                    , sourceText = Strings.mathExampleText
                    , renderedText = renderFromEditRecord model.counter editRecord
                  }
                , Cmd.none
                )


normalize : String -> String
normalize str =
    str |> String.lines |> List.filter (\x -> x /= "") |> String.join "\n"


prependMacros : String -> String -> String
prependMacros macros_ sourceText =
    "$$\n" ++ (macros_ |> normalize) ++ "\n$$\n\n" ++ sourceText


renderFromEditRecord : Int -> EditRecord (Html msg) -> Html msg
renderFromEditRecord counter editRecord =
    MiniLatex.getRenderedText editRecord
        |> List.map (\x -> Html.div [ HA.style "margin-bottom" "0.65em" ] [ x ])
        |> Html.div []


render_ : String -> Cmd Msg
render_ str =
    Task.perform Render (Task.succeed str)


render : String -> Html msg
render sourceText =
    let
        macroDefinitions =
            initialMacroText
    in
        MiniLatex.render macroDefinitions sourceText



--
-- VIEW FUNCTIONS
---


view : Model (Html Msg) -> Html Msg
view model =
    div outerStyle
        [ display model
        ]


display : Model (Html Msg) -> Html Msg
display model =
    div []
        [ h1 [ style "margin-left" "20px" ] [ text "MiniLatex Demo" ]
        , p [ style "margin-left" "20px", style "font-style" "italic" ]
            [ text "This app is a demo of the ongoing MiniLatex research project."
            , br [] []
            , text "See "
            , a [ href "https://knode.io", target "_blank" ] [ text "knode.io" ]
            , text " for a more substantial use of this technology."
            ]
        , label "Edit or write new LaTeX below. It will be rendered in real time."
        , editor model
        , renderedSource model
        , macroPanel model
        , p [ style "clear" "left", style "margin-left" "20px", style "margin-top" "-20px" ] [ clearButton 60, restoreTextButton 80, exampleButton 80, fullRenderButton 100 ]
        ]


label text_ =
    p labelStyle [ text text_ ]


editor : Model (Html msg) -> Html Msg
editor model =
    textarea (editorTextStyle ++ [ onInput GetContent, value model.sourceText ]) []


macroPanel : Model (Html msg) -> Html Msg
macroPanel model =
    Html.div []
        [ textarea (macroPanelStyle ++ [ onInput GetMacroText, value model.macroText ]) []
        , p [ style "clear" "left", style "margin-left" "20px", style "padding-top" "10px" ]
            [ text "Macros: write one macro per line (right panel)" ]
        ]


renderedSource : Model (Html msg) -> Html msg
renderedSource model =
    Html.div renderedSourceStyle
        [ model.renderedText ]



--
-- BUTTONS
--


clearButton width =
    button ([ onClick Clear ] ++ buttonStyle colorBlue width) [ text "Clear" ]


fullRenderButton width =
    button ([ onClick FullRender ] ++ buttonStyle colorBlue width) [ text "Full Render" ]


restoreTextButton width =
    button ([ onClick RestoreText ] ++ buttonStyle colorBlue width) [ text "Example 1" ]


exampleButton width =
    button ([ onClick ExampleText ] ++ buttonStyle colorBlue width) [ text "Example 2" ]
