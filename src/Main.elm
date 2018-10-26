module Main exposing (Flags, Model, Msg(..), buttonStyle, clearButton, colorBlue, colorDark, colorLight, debounceConfig, display, editor, editorTextStyle, exampleButton, fullRenderButton, init, initialMacroText, initialText, label, labelStyle, macroPanel, macroPanelStyle, main, mathExampleText, normalize, outerStyle, prependMacros, render, renderFromEditRecord, render_, renderedSource, renderedSourceStyle, restoreTextButton, subscriptions, textStyle, update, view)

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


main : Program Flags (Model (Html Msg)) Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }



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
    | ExampleText



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
            MiniLatex.initializeEditRecord 0 initialText

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
                    MiniLatex.initializeEditRecord model.seed (prependMacros initialMacroText initialText)
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
                    MiniLatex.initializeEditRecord model.seed (prependMacros initialMacroText mathExampleText)
            in
            ( { model
                | counter = model.counter + 1
                , editRecord = editRecord
                , sourceText = mathExampleText
                , renderedText = renderFromEditRecord model.counter editRecord
              }
            , Cmd.none
            )


normalize : String -> String
normalize str =
    str |> String.lines |> List.filter (\x -> x /= "") |> String.join "\n"


prependMacros : String -> String -> String
prependMacros macros_ sourceText =
    let
        macros__ =
            macros_ |> normalize
    in
    "$$\n" ++ macros__ ++ "\n$$\n\n" ++ sourceText


renderFromEditRecord : Int -> EditRecord (Html msg) -> Html msg
renderFromEditRecord counter editRecord =
    MiniLatex.getRenderedText editRecord
        |> List.map (\x -> Html.div [ HA.style "margin-bottom" "0.65em" ] [ x ])
        |> Html.div []


render_ : String -> Cmd Msg
render_ str =
    Task.perform Render (Task.succeed str)


{-| NEF
-}
render : String -> Html msg
render sourceText =
    let
        macroDefinitions =
            initialMacroText
    in
    MiniLatex.render macroDefinitions sourceText



-- VIEW FUNCTIONS


view : Model (Html Msg) -> Html Msg
view model =
    div outerStyle
        [ display model
        ]



-- DISPLAY: EDITOR AND RENDERED TEXT


display : Model (Html Msg) -> Html Msg
display model =
    div []
        [ h1 [ style "margin-left" "20px" ] [ text "MiniLatex Live" ]
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


clearButton width =
    button ([ onClick Clear ] ++ buttonStyle colorBlue width) [ text "Clear" ]


fullRenderButton width =
    button ([ onClick FullRender ] ++ buttonStyle colorBlue width) [ text "Full Render" ]


restoreTextButton width =
    button ([ onClick RestoreText ] ++ buttonStyle colorBlue width) [ text "Example 1" ]


exampleButton width =
    button ([ onClick ExampleText ] ++ buttonStyle colorBlue width) [ text "Example 2" ]


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
            width + 0 |> String.fromInt |> (\x -> x ++ "px")
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
    [ style "margin-top" "20px"
    , style "background-color" "#e1e6e8"
    , style "padding" "20px"
    , style "width" "1430px"
    , style "height" "710px"
    ]


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


initialMacroText =
    normalize """
\\newcommand{\\bra}{\\langle}
\\newcommand{\\ket}{\\rangle}

\\newcommand{\\set}[1]{\\{\\ #1 \\ \\}}
\\newcommand{\\sett}[2]{\\{\\ #1 \\ |\\ #2 \\}}
\\newcommand{\\id}{\\mathbb{\\,I\\,}}
"""


initialText =
    """



\\title{MiniLatex Live}
% \\author{James Carlson}
% \\email{jxxcarlson@gmail.com}
% \\date{August 5, 2018}s



\\maketitle

% EXAMPLE 1

\\begin{comment}
This multi-line comment
should also not
be visible in the 
rendered text.
\\end{comment}


\\tableofcontents

\\section{Introduction} 

MiniLatex is a subset of LaTeX that can be 
rendered live in the browser. Feel free to 
experiment with MiniLatex using this app 
\\mdash you can change the text in the
left-hand window, or clear it and enter 
your own text.

MiniLatex is still a research project, albeit 
advancing rapidly.  I need your feedback to make 
it better: jxxcarlson at gmail.  See \\cite{H}, 
\\cite{T} below for articles on the design of 
MiniLatex.  The source code is at \\cite{S}.  
We are using an experimental version of the 
code here that will be released in the 
Fall of 2018.

For a searchable repository of MiniLatex documents, 
see \\href{https://knode.io}{knode.io}.  
You can create, edit, and disseminate documents 
using \\strong{knode}.  For an example of a long 
MiniLatex document, see 
\\href{https://www.knode.io/424}{QF Notes} 
\\mdash then click on the title.
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

\\section{Math-mode Macros}

A little Dirac notation \\cite{D} from quantum mechanics: 

$$
  \\bra x | y \\ket = \\bra y | x \\ket.
$$

The \\strong{bra} and \\strong{ket} macros are defined 
in the panel on the right.  You can always define and 
use math-mode macros in MiniLatex.

More macros \\mdash see the right-hand panel for their 
definitions:

$A = \\set{a \\in Z, a \\equiv 1\\ mod\\ 2}$

$B = \\sett{a,b,c}{a,b,c \\in Z}$

Note that you can easily write 
about Tex in MiniLatex.  
Thus the  \\code{\\backslash{bra }} 
macro is defined as 
\\code{\\backslash{newcommand}\\texarg{\\backslash{bra}}\\texarg{\\backslash{langle}}}.

\\section{Text-mode Macros}

\\newcommand{\\hello}[1]{Hello \\strong{#1}!}

We have added an experimental macro expansion feature that permits
the author to define new text-mode macros in a MiniLatex document.
For example, if we add the text

\\begin{verbatim}
\\newcommand{\\hello}[1]{Hello \\strong{#1}!}
\\end{verbatim}

then the macro \\backslash{hello}\\texarg{John} renders as \\hello{John}

Likewiese, if we make the definition,

\\begin{verbatim}
\\newcommand{\\reverseconcat}[3]{#3#2#1} 
\\end{verbatim}

\\newcommand{\\reverseconcat}[3]{#3#2#1} 

then \\backslash{reverseconcat}\\texarg{A}\\texarg{B}\\texarg{C} = \\reverseconcat{A}{B}{C}

The macro expansion feature will need a lot more work and testing.
We also plan to add a feature so that authors can define new environments.

\\section{Theorems}

\\begin{theorem} 
There are infinitely many prime numbers.
\\end{theorem}

\\begin{theorem} 
There are infinitley many prime numbers 
$p$ such that $p \\equiv 1\\ mod\\ 4$.
\\end{theorem}

\\section{Images}

\\image{http://psurl.s3.amazonaws.com/images/jc/beats-eca1.png}{Figure 1. Two-frequency beats}{width: 350, align: center}

\\section{Lists and Tables}

A list

\\begin{itemize}

\\item This is \\strong{just} a test.

\\item \\italic{And so is this:} $a^2 + b^2 = c^2$

\\begin{itemize}

\\item Items can be nested

\\item And you can do this: 
$ \\frac{1}{1 + \\frac{2}{3}} $

\\end{itemize}

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

\\section{Errors and related matters}

\\href{http://nytimes.com}{NYT

Pythagoras is said to have said that $z^2 + b^2 = c^2

Errors, as illustrated above, are rendered in real time and are reported in red, in place. Compare
with the source text on the left. 

We plan to make error reporting still better, giving detailed context.  Note, by the way, what happens when a nonexistent macro like \\italic{hohoho } is used:

\\begin{indent}
\\hohoho{123}
\\end{indent}

This is intentional.  Note also what happens when we use a nonexistent environment like \\italic{joke}:

\\begin{joke}
Did you hear the one about the mathematician, the philosopher, and the engineer?
\\end{joke}

This default treatment of unkown environments is also intentional, and can even be useful.

\\section{Technology}

MiniLatex is written in \\href{http://elm-lang.org}{Elm}, the statically typed functional
programming language created by Evan Czaplicki.  Because of its excellent 
\\href{http://package.elm-lang.org/packages/elm-tools/parser/latest}{parser combinator library}, Elm is an ideal choice for a project like the present one.


For an overview of the design of MiniLatex, see
\\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards Latex in the Browser}.
Briefly, \\href{https://www.mathjax.org/}{MathJax} is used inside dollar signs, and Elm is used outside.

\\bigskip


\\section{References}

\\begin{thebibliography}

\\bibitem{D} \\href{https://ocw.mit.edu/courses/physics/8-05-quantum-physics-ii-fall-2013/lecture-notes/MIT8_05F13_Chap_04.pdf}{On Dirac's bra-ket notation}, MIT Courseware.

\\bibitem{G} James Carlson, \\href{https://knode.io/628}{MiniLaTeX Grammar},

\\bibitem{H} James Carlson, \\href{https://hackernoon.com/towards-latex-in-the-browser-2ff4d94a0c08}{Towards LaTeX in the Browser }, Hackernoon

\\bibitem{S} \\href{http://package.elm-lang.org/packages/jxxcarlson/minilatex/latest}{Source code}

\\bibitem{T} James Carlson, \\href{https://knode.io/525}{MiniLatex Technical Report}

\\end{thebibliography}





"""


mathExampleText =
    """
% EXAMPLE 2

\\setcounter{section}{8}


\\section{Bras and Kets}

Paul Dirac invented a new notation \\mdash the notation of bras and kets \\mdash for working with hermitian inner products and operators on a Hilbert space $H$.  We describe the basics below, then elaborate on how these apply in the case of countable and continuous orthonormal bases.  We also discuss some of the mathematical issues raised by the delta function and by formulas such as

\\begin{equation}
\\int_{-\\infty}^\\infty \\frac{dx}{2\\pi}\\; | x \\ket\\bra x | = 1
\\end{equation}
a
that Dirac introduced and which abound in the physics literature.

\\subsection{The basics}

For the inner product of two vectors, we write $\\bra u | v \\ket$, where this expression is linear in the second variable and conjugate linear in the first.  Thus Dirac's $\\bra u | v \\ket$ is the mathematicians's $\\bra v, w \\ket$.  

In this notations, $\\bra u | c v\\ket = c\\bra u | v \\ket$, but $\\bra cu |  v\\ket = \\bar c\\bra u | v \\ket$. The symmetry condition reads $\\bra v | u \\ket = \\overline{\\bra u  | v \\ket}$.  

Let $A$ be an operator.  The expression $\\bra u | A | v \\ket$ means $\\bra u | Av \\ket$.  Thus it is the same as $\\bra A^*u | v \\ket$.  

The symbol $|v\\ket$ has the same meaning as $v$ if $v$ is a vector. Such vectors are called \\term{kets}.  Let $\\set{ \\psi_a }$ be a set of states, that is, eigenvectors for an operator $A$ with eigenvalue $a$.  The notations $\\psi_a$, $|\\psi_a \\ket$, and  $|a\\ket$ all stand fors the same thing.  It makes sense to say

\\begin{equation}
A|a\\ket = a|a\\ket
\\end{equation}

The expression $\\bra u |$ is the \\term{bra} associated to $u$, where $u$ is an element of the space \\emph{dual} to $V$.  That is, $u$ is a linear functional on $V$:

\\begin{equation}
\\bra u |(v) = u(v)
\\end{equation}

If $u$ is a vector in $V$, we may define the linear functional $\\phi_u(v) = \\bra u | v \\ket$.  In this case, both sides of the equation

\\begin{equation}
\\bra u |(v) = \\bra u | v \\ket
\\end{equation}

have meaning.  For finite-dimensional spaces, we may view bras as row vectors and kets as column vectors.

Consider next the expression $| v \\ket \\bra v |$ where $v$ is a unit vector. it operates on a arbitrary vector $w$ by the rule

\\begin{equation}
  | w \\ket \\mapsto | v \\ket \\bra v | w \\ket
\\end{equation}

If $| w \\ket$ is orthogonal to $| v \\ket$, then the result is zero.  If $| w \\ket = | v \\ket$, then the result is $| v \\ket$, Therefore $| v \\ket \\bra v |$ is orthogonal projection onto $v$.


\\subsection{Resolution of the identity}

Let $\\set{ |v_n\\ket} = \\set{ |n\\ket}$ be a complete orthonormal set for $H$ which is indexed by integers $n = 0, 1, 2, \\ldots$.  We claim that

\\begin{equation}
\\label{resolutionofidentity}
\\sum_n | n \\ket \\bra n | = \\id.
\\end{equation}

That is, the expression on the left, which is a sum of projections operators, is the identity operator $\\id$.  We say that the left-hand side of \\eqref{resolutionofidentity} is  \\term{resolution of the identity}.  The proof that  \\eqref{resolutionofidentity} holds an exercise in bra-ket formalism. Let $v$ be arbitrary and write

\\begin{equation}
  v = \\sum_m | m \\ket\\bra m | v \\ket 
\\end{equation}

This is the Fourier decomposition of $v$.  Note that it depends linearly on $| v \\ket$. Applying $\\sum | n \\ket\\bra n|$ to $v$, we find that 

\\begin{align}
\\left(\\sum_n  | n\\ket \\bra n |\\right) \\left(\\sum_m | m \\ket\\bra m | v \\ket \\right) &=
\\sum_{m,n}  | n \\ket \\bra n |m \\ket\\bra m | v \\ket  \\\\
&= \\sum_{m,n}  | n \\ket \\delta_{n,m}\\bra m | v \\ket \\\\
& = \\sum_{m}  | m \\ket \\bra m | v \\ket \\\\
&= v
\\end{align}

\\strong{Q.E.D.}

Here the quantities $\\delta_{n,m}$ may be viewed as the elements of the identity matrix  \\mdash possibly $n\\times n$, possibly $\\infty\\times\\infty$.

Consider next an operator $\\Omega$.  Write it as

\\begin{align}
\\Omega &= \\left(\\sum_m  | m \\ket \\bra m |\\right) \\Omega \\left(\\sum_n  | n \\ket \\bra n | \\right) \\\\
&= \\sum_{m,n}  |m | \\ket \\bra m |  \\Omega | n \\ket \\bra n | & \\\\
& = \\sum_m  | m\\ket   \\Omega_{m.n} \\bra n |
\\end{align}

The operator  is determined by its \\term{matrix elements}

\\begin{equation}
\\Omega_{m,n} = \\bra m |  \\Omega | n \\ket
\\end{equation}

The matrix elements are the elements o the matrix of  $\\Omega$ in the given orthonormal basis, i.e.,  the $\\Omega_{m,n}$

One often encounters the phrase "let us insert a resolution of the identity".  To see what this means, consider the expression $\\bra m AB m \\ket$, which we expand as follows:

\\begin{align}
\\bra m | AB n | \\ket &= \\bra m | A\\id B | n \\ket \\\\
&= \\sum_i \\bra m A | i \\ket \\bra i | B n \\ket
\\end{align}

This is the same as the identity

\\begin{equation}
(AB)_{mn} = \\sum_i A_{mi} B_{in}
\\end{equation}

\\subsection{Continuous spectrum}

One also has resolution of the identity for operators with continuous spectrum.  Take, for example, the operator $-id/dx$ which has (generalized) eigenfunctions $e^{ikx}$. Writing $| k \\ket$ for $e^{ikx}$, one has

\\begin{equation}
\\id = \\frac{1}{2\\pi} \\int_{-\\infty}^\\infty dk\\, | k \\ket \\bra k |  
\\end{equation}

Standing by itself, the meaning of this formula is a subtle matter.  However, when applied to a function, we have

\\begin{equation}
| f \\ket = \\frac{1}{2\\pi} \\int_{-\\infty}^\\infty dk\\, | k \\ket \\bra k | f \\ket 
\\end{equation}

This is an encoding of the Fourier inversion theorem:

\\begin{equation}
f(x) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^\\infty \\hat f(k) e^{ikx} dk
\\end{equation}




\\subsection{References}

\\href{http://ocw.mit.edu/courses/physics/8-05-quantum-physics-ii-fall-2013/lecture-notes/MIT8_05F13_Chap_04.pdf}{Dirac's Bra and Ket notation} \\mdash Notes from B. Zwiebach's course at MIT

\\href{http://www.physics.iitm.ac.in/~labs/dynamical/pedagogy/vb/delta.pdf}{All about the Dirac delta function} \\mdash V. Balakrishnan, IIT Madras

\\href{http://math.arizona.edu/~kglasner/math456/fouriertransform.pdf}{Fourier transform techniques} \\mdash U. Arizona notes

\\href{https://www.math.utah.edu/~gustafso/s2013/3150/pdeNotes/fourierTransorm-PeterOlver2013.pdf}{Fourier transform} \\mdash Olver notes

\\href{http://www.physics.rutgers.edu/~steves/501/Lectures_Final/Lec06_Propagator.pdf}{Free particle propagator}


"""
