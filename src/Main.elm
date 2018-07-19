module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Encode
import MeenyLatex.Differ exposing (EditRecord)
import MeenyLatex.MiniLatex as MiniLatex
import Debounce exposing(Debounce)
import Task
import Random


main =
    Browser.element { view = view, update = update, init = init, subscriptions = subscriptions }


-- TYPES

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

-- This defines how the debouncer should work.
-- Choose the strategy for your use case.
debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later 250
  , transform = DebounceMsg
  }

type alias Flags =
    {}


-- MAIN FUNCTIONS

init : Flags -> ( Model (Html msg), Cmd Msg )
init flags =
    let
        editRecord =
            MiniLatex.setup 0 initialText
        
        model =
            { sourceText = initialText
            , macroText = initialMacroText
            , renderedText = render (prependMacros initialMacroText initialText)
            , editRecord = editRecord
            , debounce = Debounce.init
            , counter = 0
            , seed = 0
            }
    
    in
    ( model, Cmd.none )


subscriptions : Model (Html msg) -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model (Html msg) -> ( Model (Html msg), Cmd Msg )
update msg model =
    case msg of

        GetContent str ->
            let
                (debounce, cmd) =
                   Debounce.push debounceConfig str model.debounce
            in
                ({ model
                    | sourceText = str
                    , debounce = debounce
                    }
                    , cmd)

        GetMacroText str ->
            ({ model | macroText = str }, Cmd.none)
                    
        DebounceMsg msg_ ->
            let
                (debounce, cmd) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast render_)
                        msg_
                        model.debounce
            in
                ({ model | debounce = debounce }, cmd)  


        Render str ->
          let 
            n = String.fromInt model.counter

            newEditRecord = 
                    (MiniLatex.update model.seed  model.editRecord (prependMacros model.macroText str))
          in
            ({ model | 
                editRecord = newEditRecord
                , renderedText = renderFromEditRecord model.counter newEditRecord 
                , counter = model.counter + 1
             }
            , Random.generate NewSeed (Random.int 1 10000))

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear -> 
           let 
                editRecord =
                   MiniLatex.setup 0 ""  
           in
             ( { model | 
                sourceText = ""
                , editRecord = editRecord
                , renderedText = renderFromEditRecord model.counter editRecord 
                , counter = model.counter + 1
                }
             , Cmd.none)


        FullRender ->    
          let 
            editRecord =
              MiniLatex.setup model.seed  (prependMacros model.macroText model.sourceText)
          in 
            ( { model | counter = model.counter + 1
              , editRecord = editRecord
              , renderedText = renderFromEditRecord model.counter editRecord }
            , Cmd.none)

        RestoreText ->    
          let 
            editRecord =
             MiniLatex.setup model.seed (prependMacros initialMacroText initialText )
          in 
            ( { model | counter = model.counter + 1
              , editRecord = editRecord
              , sourceText =  initialText
              , renderedText = renderFromEditRecord model.counter editRecord }
            , Cmd.none)



normalize str = 
  str |> String.lines |> List.filter (\x -> x /= "") |> String.join("\n") 

   
prependMacros macros_ sourceText = 
  let
    macros__ =  (macros_ |> normalize)
  in
    "$$\n" ++ macros__ ++ "\n$$\n\n" ++ sourceText 

renderFromEditRecord : Int -> EditRecord (Html msg)-> Html msg
renderFromEditRecord counter editRecord =
    MiniLatex.getRenderedText editRecord
        |> List.map (\x -> Html.div [ HA.style "margin-bottom" "0.65em" ] [  x ])
        |> Html.div []


render_ : String -> Cmd Msg
render_ str =
    Task.perform Render (Task.succeed str)

{-| NEF -}
render : String -> Html msg 
render sourceText =
  let 
    macroDefinitions = initialMacroText
  in 
    MiniLatex.render macroDefinitions sourceText


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
    , p [style "margin-left" "20px", style "font-style" "italic"] [text "This app is a demo of the ongoing MiniLatex research project."]
    , label "Edit or write new LaTeX below. It will be rendered in real time."
    , editor model
    , renderedSource model
    , macroPanel model 
    , p [style "clear" "left", style "margin-left" "20px", style "margin-top" "-20px"] [clearButton 60, restoreTextButton 80, fullRenderButton 100 ]
  ]



label text_ =
    p labelStyle [ text text_ ]
  
  
editor model =
  textarea (editorTextStyle ++ [ onInput GetContent, value model.sourceText ] )  [  ]

macroPanel model =
  Html.div [] [
  textarea (macroPanelStyle ++ [ onInput GetMacroText, value model.macroText ] )  [  ]
  , p [style "clear" "left", style "margin-left" "20px", style "padding-top" "10px"]  
    [text "Macros: write one macro per line (right panel)"]
  ]


renderedSource : Model (Html msg) -> Html msg
renderedSource model =
        Html.div renderedSourceStyle
           [ model.renderedText]


clearButton width =
    button ([ onClick Clear ] ++ buttonStyle colorBlue width) [ text "Clear" ]

fullRenderButton width =
    button ([ onClick FullRender ] ++ buttonStyle colorBlue width) [ text "Full Render" ]

restoreTextButton width =
    button ([ onClick RestoreText ] ++ buttonStyle colorBlue width) [ text "Restore" ]


colorBlue =
    "rgb(100,100,200)"


colorLight =
    "#88a"


colorDark =
    "#444"


buttonStyle : String -> Int -> List (Html.Attribute msg)
buttonStyle color width =
    let
        realWidth =
            width + 0 |> String.fromInt |> \x -> x ++ "px"
    in
        [ style "backgroundColor" color
        , style "color" "white"
        , style "width" realWidth
        , style "height" "25px"
        , style "margin-top" "20px"
        , style "margin-right" "12px"
        , style "font-size" "9pt"
        , style "text-align" "center"
        , style "border" "none"
        ]
-- STYLE FUNCTIONS

outerStyle = 
  [style "margin-top" "20px"
  , style "background-color" "#e1e6e8"
  , style "padding" "20px"
  , style "width" "1430px"
  , style "height" "690px"]

editorTextStyle =
    textStyle "400px" "450px" "#fff"

macroPanelStyle =
    textStyle "300px" "450px" "#fff"

renderedSourceStyle =
    textStyle "400px" "450px" "#fff"


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


-- TEXT

initialMacroText = normalize """
\\newcommand{\\bra}{\\langle}
\\newcommand{\\ket}{\\rangle}

\\newcommand{\\set}[1]{\\{\\ #1 \\ \\}}
\\newcommand{\\sett}[2]{\\{\\ #1 \\ |\\ #2 \\}}
"""

initialText = 
    """

\\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards LaTeX in the Browser (Hackernoon)}

\\href{https://jxxcarlson.github.io/#minilatex}{MiniLaTeX at github.io}

\\tableofcontents

\\section{Formulas}

\\italic{Try editing formula \\eqref{integral:xn} or \\eqref{integral:exp} below.}

The most basic integral:

\\begin{equation}
\\label{integral:xn}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

An improper integral:

\\begin{equation}
\\label{integral:exp}
\\int_0^\\infty e^{-x} dx = 1
\\end{equation}

\\section{Macros}

A little Dirac notation from quantum mechanics: 

$$
  \\bra x | y \\ket = \\bra y | x \\ket.
$$

The \\strong{bra} and \\strong{ket} macros are defined in the panel
on the right.  You can always define and use math-mode macros in
MiniLatex.

More macros:

$A = \\set{a \\in Z, a \\equiv 1\\ mod\\ 2}$

$B = \\sett{a,b,c}{a,b,c \\in Z}$

\\section{Theorems}

\\begin{theorem} 
There are infinitely many prime numbers.
\\end{theorem}

\\begin{theorem} 
There are infinitley many prime numbers 
$p$ such that $p \\equiv 1\\ mod\\ 4$.
\\end{theorem}

\\section{Images}

\\image{http://psurl.s3.amazonaws.com/images/jc/beats-eca1.png}{Two-frequency beats}{width: 350, float: right}

\\section{Lists and Tables}

A list

\\begin{itemize}

\\item This is \\strong{just} a test.

\\item And so is this: $a^2 + b^2 = c^2$

\\end{itemize}

A table 

\\begin{indent}
\\strong{Light Elements}
\\begin{tabular}{ l l l l }
Hydrogen & H & 1 & 1.008 \\\\
Helium & He & 2 & 4.003 \\\\
Lithium& Li & 3 & 6.94 \\\\
Beryllium& Be& 4& 9.012 \\\\
\\end{tabular}
\\end{indent}



\\section{Notes}

MiniLatex is still a research project, but is getting closer to its first release. Please
bear in mind that there are some rough edges. For example, the links in the
table of contents don't work.  Lots to do! 

I will try to extend the  coverage of LaTeX as time and energy permit.  
Please send suggestions in this regard as well
as bug reports and comments in general to jxxcarlson at gmail.

\\section{Technology}

MiniLatex is written in \\href{http://elm-lang.org}{Elm}, a statically typed functional
programming language with an excellent 
\\href{http://package.elm-lang.org/packages/elm-tools/parser/latest}{parser combinator library}.
For an overview of the design of MiniLatex, see
\\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards Latex in the Browser}.
Briefly, \\href{https://www.mathjax.org/}{MathJax} is used inside dollar signs, and Elm is used outside.

Code for MiniLatex is open source.  See the latest release at
\\href{http://package.elm-lang.org/packages/jxxcarlson/minilatex/latest}{package.elm-lang.org},
and note the link to the GitHub repository.
MiniLatex Live uses an unreleased experimental version of this package.  Coming soon!

For a more thoroughgoing use of MiniLatex, see \\href{http://www.knode.io}{www.knode.io} --
a site for class and lecture notes, etc.

\\bigskip



"""


