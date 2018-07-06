module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode
import MeenyLatex.Differ exposing (EditRecord)
import MeenyLatex.Driver


main =
    Browser.element { view = view, update = update, init = init, subscriptions = subscriptions }


-- TYPES

type alias Model a =
    { sourceText : String
    , renderedText : a
    , counter : Int
    }

type Msg
    = Clear
    | Render
    | GetContent String


type alias Flags =
    {}

render : String -> Html msg 
render sourceText =
  let 
    macroDefinitions = ""
  in 
    MeenyLatex.Driver.render macroDefinitions sourceText

-- MAIN FUNCTIONS

init : Flags -> ( Model (Html msg), Cmd Msg )
init flags =
    let
        initialText = "$$\\int_0^1 x^n dx = \\frac{1}{n+1}$$"
        model =
            { sourceText = initialText
            , renderedText = render initialText
            , counter = 0
            }
    
    in
    ( model, Cmd.none )


subscriptions : Model (Html msg) -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model (Html msg) -> ( Model (Html msg), Cmd Msg )
update msg model =
    case msg of
        Render ->
            ( { model
                | renderedText = render model.sourceText,
                  counter = model.counter + 1
              }
            , Cmd.none
            )

        Clear ->
            ({ model | sourceText = ""
              , renderedText = render ""
              , counter = model.counter + 1
            }, Cmd.none)


        GetContent str ->
            ( { model | sourceText = str
                       , renderedText = render str
                       , counter = model.counter + 1 }
            , Cmd.none )


-- VIEW FUNCTIONS 


view model =
    div outerStyle
        [ 
            display model
        ]
    

-- DISPLAY: EDITOR AND RENDERED TEXT 

display model = 
  div [ ] [
    label "Type LaTeX below. It will be rendered in real time."
    , editor model
    , renderedSource model
  ]

label text_ =
    p labelStyle [ text text_ ]


editor model =
   textarea (editorTextStyle ++ [ onInput GetContent, value model.sourceText ] ) 
      [  ]


renderedSource : Model (Html msg) -> Html msg
renderedSource model =
        Keyed.node "div" renderedSourceStyle
           [((String.fromInt model.counter), model.renderedText)]


-- STYLE FUNCTIONS

outerStyle = 
  [style "margin-top" "20px"
  , style "background-color" "#ddd"
  , style "padding" "20px"
  , style "width" "930px"
  , style "height" "340px"]

editorTextStyle =
    textStyle "400px" "250px" "#fff"

renderedSourceStyle =
    textStyle "400px" "250px" "#fff"


textStyle width height color =
    [ style "width" width
    , style "height" height
    , style "padding" "15px"
    , style "margin-left" "20px"
    , style "background-color" color
    , style "overflow" "scroll"
    , style "float" "left"
    ]

labelStyle =
    [ style "margin-top" "5px"
    , style "margin-bottom" "0px"
    , style "margin-left" "20px"
    , style "font-weight" "bold"
    ]





