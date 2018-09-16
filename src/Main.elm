module Main exposing (main)

import Article
import Browser
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
    Example.tweet897597535254130690


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Data


type Model
    = Loading
    | Page Image Preset


type alias Image =
    { colorscheme : Colorscheme
    , progress : Float
    , system : System
    , controls : Controls.State
    }


type Preset
    = Koch
    | Dragon
    | Plant
    | Triangles
    | Mesh
    | Flower
    | Eyes


type Msg
    = Randomize
    | Build Colorscheme
    | ShowEditor
    | CloseEditor
    | ChangeProgress Float
    | ChangeAngle String
    | ChangeIterations String
    | SelectPreset Preset


init : flags -> ( Model, Cmd Msg )
init flags =
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
            ( model, Random.generate Build Colorscheme.random )

        Build colorscheme ->
            let
                image =
                    { colorscheme = colorscheme
                    , progress = 1
                    , system = example
                    , controls = Controls.state
                    }
            in
            ( Page image Koch, Cmd.none )

        ChangeProgress progress ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image preset ->
                    let
                        page =
                            Page { image | progress = progress } preset
                    in
                    ( page, Cmd.none )

        SelectPreset preset ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image _ ->
                    let
                        system =
                            case preset of
                                Koch ->
                                    Example.koch

                                Dragon ->
                                    Example.dragon

                                Plant ->
                                    Example.plant

                                Triangles ->
                                    Example.tweet896797261471989760

                                Mesh ->
                                    Example.tweet897839129299374082

                                Flower ->
                                    Example.tweet897597535254130690

                                Eyes ->
                                    Example.eyes
                    in
                    ( Page { image | system = system } preset, Cmd.none )

        ChangeIterations string ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image preset ->
                    let
                        iterations =
                            String.toInt string
                                |> Maybe.withDefault image.system.iterations

                        { system } =
                            image

                        newSystem =
                            { system | iterations = iterations }

                        page =
                            Page { image | system = newSystem } preset
                    in
                    ( page, Cmd.none )

        ChangeAngle string ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image preset ->
                    let
                        angle =
                            String.toInt string
                                |> Maybe.withDefault image.system.angle

                        { system } =
                            image

                        newSystem =
                            { system | angle = angle }

                        page =
                            Page { image | system = newSystem } preset
                    in
                    ( page, Cmd.none )

        ShowEditor ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image preset ->
                    ( Page { image | controls = image.controls |> Controls.show } preset, Cmd.none )

        CloseEditor ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image preset ->
                    ( Page { image | controls = image.controls |> Controls.hide } preset, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    case model of
        Loading ->
            { title = "L-System Builder"
            , body = [ Html.text "loading..." ]
            }

        Page image preset ->
            { title = "L-System Builder"
            , body =
                [ Article.frameWithMarkdownBody
                    image.controls.visible
                    (mastheadView image preset)
                    ""
                ]
            }


systemConfig : System.Config Msg
systemConfig =
    System.config { onSwipe = ChangeProgress }


mastheadView : Image -> Preset -> List (Html Msg)
mastheadView image preset =
    let
        state =
            { colorscheme = image.colorscheme
            , system = image.system
            , progress = image.progress
            }
    in
    [ Html.Lazy.lazy2 System.view systemConfig state
    , controlsView image preset
    ]


controlsView : Image -> Preset -> Html Msg
controlsView image selectedPreset =
    let
        { system } =
            image

        controlsConfig =
            Controls.config
                { onOpen = ShowEditor
                , onDismiss = CloseEditor
                , title = "l-system builder"
                }

        controlFor iterations =
            Controls.text <|
                String.fromInt iterations
                    ++ String.fromList (System.expand { system | iterations = iterations })

        state =
            image.controls

        -- What the production looks like after so many iterations
        expansions =
            List.range 0 system.iterations
                |> List.map controlFor

        rulesToString char production dict =
            Dict.insert (String.fromChar char) (String.fromList production) dict

        rulesDict =
            Dict.foldl rulesToString Dict.empty system.rules

        presets =
            [ ( "Koch Curve", Koch )
            , ( "Dragon Curve", Dragon )
            , ( "Plant", Plant )
            , ( "Triangles", Triangles )
            , ( "Mesh", Mesh )
            , ( "Flower", Flower )
            , ( "Eyes", Eyes )
            ]

        controls =
            [ Controls.union "preset" presets selectedPreset SelectPreset
            , Controls.string "start rule" (String.fromList system.start)
            , Controls.dict "rules" rulesDict
            , Controls.int "angle (degrees)" system.angle ChangeAngle
            , Controls.int "iterations" system.iterations ChangeIterations
            , Controls.text "valid characters in the rules include [ (add a new level on the stack), ] (pop a level off the stack), + (turn clockwise by given angle), - (counterclockwise), or another rule. The rules above will get expanded into:"
            ]
    in
    Controls.view controlsConfig state controls


prose : String
prose =
    """
**Tell me how to build a tree.**

Not a shoe tree, not a decision tree. One of those great big leafy ones you get in the forest. What kind of instructions could you write down to make that branching structure? What would you do in the first year of the tree's life? The second?

Biologist Aristid Lindenmayer asked himself this in the 1960s, staring at the growth patterns of bacteria, yeast, and other fungi. What patterns made up their structure? Which parts could be considered repeating?

Eventually Lindenmayer would define a means of expressing those growth patterns, as well as the patterns of many plants, trees, and more abstract shapes through a shorthand represensation that could express wildly differing structures from a set of simple rules. The main insights were:

- Each system has an initial state of symbols
- Those symbols can be divided into two groups: ones that can be substituted within that original state (variables) and ones that remain fixed (constants). This is known as the alphabet.
- The system must also contain a set of rules for substituing variables with a combination of more symbols. One variable cooresponds to one or more symbols in the alphabet that. These are known as productions.
"""
