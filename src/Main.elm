module Main exposing (main)

import Article
import Color exposing (Color)
import Colorscheme exposing (Colorscheme)
import Controls
import Dict exposing (Dict)
import Example
import Html exposing (Html)
import Html.Lazy
import Random
import System exposing (System)
import Task


example : System
example =
    Example.tweet896797261471989760


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Data


type Model
    = Loading
    | Page Image


type alias Image =
    { colorscheme : Colorscheme
    , progress : Float
    , system : System
    , editor : Bool
    }


type Msg
    = Randomize
    | Build Color
    | ShowEditor
    | CloseEditor
    | ChangeProgress Float
    | ChangeAngle String
    | ChangeIterations String


init : ( Model, Cmd Msg )
init =
    let
        loadCommand =
            Task.succeed Randomize |> Task.perform identity
    in
    ( Loading, loadCommand )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, Random.generate Build randomColor )

        Build color ->
            let
                image =
                    { colorscheme = Colorscheme.complementary color
                    , progress = 1
                    , system = example
                    , editor = False
                    }
            in
            ( Page image, Cmd.none )

        ChangeProgress progress ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    let
                        page =
                            Page { image | progress = progress }
                    in
                    ( page, Cmd.none )

        ChangeIterations string ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    let
                        iterations =
                            String.toInt string
                                |> Result.withDefault image.system.iterations

                        { system } =
                            image

                        newSystem =
                            { system | iterations = iterations }

                        page =
                            Page { image | system = newSystem }
                    in
                    ( page, Cmd.none )

        ChangeAngle string ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    let
                        angle =
                            String.toInt string
                                |> Result.withDefault image.system.angle

                        { system } =
                            image

                        newSystem =
                            { system | angle = angle }

                        page =
                            Page { image | system = newSystem }
                    in
                    ( page, Cmd.none )

        ShowEditor ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    ( Page { image | editor = True }, Cmd.none )

        CloseEditor ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    ( Page { image | editor = False }, Cmd.none )


{-| Generator for a random pleasing color.
-}
randomColor : Random.Generator Color
randomColor =
    let
        hue =
            Random.map degrees (Random.float 0 360)

        saturation =
            Random.float 0.1 0.9

        lightness =
            Random.float 0.1 0.9
    in
    Random.map3 Color.hsl hue saturation lightness



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "loading..."

        Page image ->
            Article.frame image.editor (mastheadView image) prose


systemConfig : System.Config Msg
systemConfig =
    System.config { onSwipe = ChangeProgress }


mastheadView : Image -> List (Html Msg)
mastheadView image =
    let
        state =
            { colorscheme = image.colorscheme
            , system = image.system
            , progress = image.progress
            }
    in
    [ Html.Lazy.lazy2 System.view systemConfig state
    , controlsView image
    ]


controlsView : Image -> Html Msg
controlsView { system, editor } =
    let
        controlsConfig =
            Controls.config
                { onOpen = ShowEditor
                , onDismiss = CloseEditor
                , title = "l-system builder"
                }

        state =
            { visible = editor }

        controlFor iterations =
            Controls.string
                (toString iterations)
                (String.fromList (System.expand { system | iterations = iterations }))

        -- What the production looks like after so many iterations
        expansions =
            List.range 0 system.iterations
                |> List.map controlFor

        rulesToString char production dict =
            Dict.insert (toString char) (String.fromList production) dict

        rulesDict =
            Dict.foldl rulesToString Dict.empty system.rules

        controls =
            [ Controls.string "start rule" (String.fromList system.start)
            , Controls.dict "rules" rulesDict
            , Controls.int "angle (degrees)" system.angle ChangeAngle
            , Controls.int "number of iterations" system.iterations ChangeIterations
            , Controls.text "valid characters in the rules include [ (add a new level on the stack), ] (pop a level off the stack), + (turn clockwise by given angle), - (counterclockwise), or another rule. The rules above will get expanded into:"
            ]
                ++ expansions
    in
    Controls.view controlsConfig state controls


prose : String
prose =
    """
## What is an L-system?

An L-system or Lindenmayer system is a parallel rewriting system and a type of formal grammar. An L-system consists of an alphabet of symbols that can be used to make strings, a collection of production rules that expand each symbol into some larger string of symbols, an initial "axiom" string from which to begin construction, and a mechanism for translating the generated strings into geometric structures. L-systems were introduced and developed in 1968 by Aristid Lindenmayer, a Hungarian theoretical biologist and botanist at the University of Utrecht. Lindenmayer used L-systems to describe the behaviour of plant cells and to model the growth processes of plant development. L-systems have also been used to model the morphology of a variety of organisms[1] and can be used to generate self-similar fractals such as iterated function systems.

## Origins

As a biologist, Lindenmayer worked with yeast and filamentous fungi and studied the growth patterns of various types of algae, such as the cyanobacteria Anabaena catenula. Originally the L-systems were devised to provide a formal description of the development of such simple multicellular organisms, and to illustrate the neighbourhood relationships between plant cells. Later on, this system was extended to describe higher plants and complex branching structures.

## L-system structure

The recursive nature of the L-system rules leads to self-similarity and thereby, fractal-like forms are easy to describe with an L-system. Plant models and natural-looking organic forms are easy to define, as by increasing the recursion level the form slowly 'grows' and becomes more complex. Lindenmayer systems are also popular in the generation of artificial life.

L-system grammars are very similar to the semi-Thue grammar (see Chomsky hierarchy). L-systems are now commonly known as parametric L systems, defined as a tuple
"""
