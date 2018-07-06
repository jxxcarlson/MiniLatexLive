module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode
import MeenyLatex.Differ exposing (EditRecord)
import MeenyLatex.Driver
import Debounce exposing(Debounce)


main =
    Browser.element { view = view, update = update, init = init, subscriptions = subscriptions }


-- TYPES

type alias Model a =
    { sourceText : String
    , renderedText : a
    , debounce : Debounce a 
    , counter : Int
    }

type Msg
    = Clear
    | Render
    | GetContent String
    | DebounceMsg Debounce.Msg

-- This defines how the debouncer should work.
-- Choose the strategy for your use case.
debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later 250
  , transform = DebounceMsg
  }

type alias Flags =
    {}

render : String -> Html msg 
render sourceText =
  let 
    macroDefinitions = ""
  in 
    MeenyLatex.Driver.render macroDefinitions sourceText

-- MAIN FUNCTIONS

initialText = 
    """
\\strong{Example}

$$\\int_0^1 x^n dx = \\frac{1}{n+1}$$


\\italic{This formula is now taught in High School.}
"""

init : Flags -> ( Model (Html msg), Cmd Msg )
init flags =
    let
        
        model =
            { sourceText = initialText
            , renderedText = render initialText
            , debounce = Debounce.init
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
            let
                -- Push your values here.
                (debounce, cmd) =
                   Debounce.push debounceConfig str model.debounce
            in
                ({ model
                    | sourceText = str
                    , debounce = debounce
                    }
                    , cmd)
                    
        DebounceMsg msg_ ->
            let
                (debounce, cmd) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        msg_
                        model.debounce
            in
                ({ model | debounce = debounce }, cmd)   
            -- ( { model | sourceText = str
            --            , renderedText = render str
            --            , counter = model.counter + 1 }
            -- , Cmd.none )


-- VIEW FUNCTIONS 


view model =
    div outerStyle
        [ 
            display model
        ]
    

-- DISPLAY: EDITOR AND RENDERED TEXT 

display model = 
  div [ ] [
      h1 [style "margin-left" "20px"] [text "MiniLatex Live"]
    , label "Type LaTeX below. It will be rendered in real time."
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
  , style "height" "410px"]

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





