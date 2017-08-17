module Main exposing (main)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Events
import Svg.Path
import Task


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
    , canvas : { width : Int }
    , system : System
    , editor : Bool
    }


type alias Rules =
    Dict Char Production


type alias Angle =
    Int


type alias Production =
    List Char


type alias System =
    { start : Production
    , angle : Angle
    , iterations : Int
    , rules : Rules
    }


type Operation
    = Forward
    | RotateClockwise
    | RotateCounterClockwise
    | StackPush
    | StackPop
    | Expand String


type alias Colorscheme =
    { background : Color, foreground : Color }


type Msg
    = Randomize
    | Build Color
    | ShowEditor
    | CloseEditor
    | ChangeProgress Float


init : ( Model, Cmd Msg )
init =
    let
        loadCommand =
            Task.succeed Randomize |> Task.perform identity
    in
    ( Loading, loadCommand )


parseAsSystem :
    { start : String
    , angle : Int
    , iterations : Int
    , rules : List ( Char, String )
    }
    -> System
parseAsSystem definition =
    let
        start =
            String.toList definition.start

        rules =
            definition.rules
                |> List.map (\( char, str ) -> ( char, String.toList str ))
                |> Dict.fromList
    in
    { definition | rules = rules, start = start }


systemOne : System
systemOne =
    parseAsSystem
        { start = "NSH"
        , angle = 90
        , iterations = 5
        , rules =
            [ ( 'F', "H-[F[-]]" )
            , ( 'D', "NNHF" )
            , ( 'N', "[]S-HFSHF" )
            , ( 'H', "[-[[N]DS-]]" )
            , ( 'S', "[N-+SHDN]" )
            ]
        }


systemTwo : System
systemTwo =
    parseAsSystem
        { start = "S"
        , angle = 60
        , iterations = 6
        , rules =
            [ ( 'F', "JS" )
            , ( 'O', "F+F+FF" )
            , ( 'L', "JF++" )
            , ( 'V', "JL+-OSFF" )
            , ( 'J', "OF+" )
            , ( 'S', "[+S]LS+V" )
            ]
        }


systemThree : System
systemThree =
    parseAsSystem
        { start = "ZZ"
        , angle = 45
        , iterations = 5
        , rules =
            [ ( 'F', "-ZFFFF-FF" )
            , ( 'Z', "PA" )
            , ( 'P', "+AZ" )
            , ( 'A', "F+[ZF]AZ" )
            ]
        }


systemFour : System
systemFour =
    parseAsSystem
        { start = "FFPF"
        , angle = 60
        , iterations = 2
        , rules =
            [ ( 'F', "PF++F[FF-F+PF+FPP][F]FFPF" )
            , ( 'P', "" )
            ]
        }


systemFive : System
systemFive =
    parseAsSystem
        { start = "TOT"
        , angle = 60
        , iterations = 2
        , rules =
            [ ( 'F', "O+" )
            , ( 'H', "F+F+" )
            , ( 'O', "T[OFF]+FH" )
            , ( 'T', "FF+HOTO" )
            ]
        }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, Random.generate Build randomColor )

        Build color ->
            let
                image =
                    { colorscheme = toColorscheme color
                    , progress = 1
                    , system = systemFive
                    , canvas = { width = 100 }
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


{-| Builds a complementary background color for the given foreground color,
ensuring that there's enough contrast between the two.
-}
toColorscheme : Color -> Colorscheme
toColorscheme color =
    let
        { hue, saturation, lightness } =
            Color.toHsl color

        normalizedLightness =
            if abs (0.5 - lightness) < 0.2 then
                lightness - 0.3
            else
                lightness

        background =
            Color.hsl hue saturation normalizedLightness

        foreground =
            Color.hsl hue saturation (1 - normalizedLightness)
    in
    Colorscheme background foreground


expand : System -> Production
expand system =
    if system.iterations < 1 then
        system.start
    else
        let
            expansionFor char =
                Dict.get char system.rules
                    |> Maybe.withDefault []
        in
        expand
            { system
                | start = List.concatMap expansionFor system.start
                , iterations = system.iterations - 1
            }


type alias Stack a =
    List a


push : a -> Stack a -> Stack a
push item stack =
    item :: stack


pop : Stack a -> Maybe ( a, Stack a )
pop stack =
    case stack of
        [] ->
            Nothing

        item :: stack ->
            Just ( item, stack )


toPath : Int -> Production -> Svg.Path.Subpath
toPath angle items =
    Svg.Path.subpath
        (Svg.Path.startAt ( 0, 0 ))
        Svg.Path.open
        (toPathHelp items { current = start, stack = [] })


type Position
    = Position Point Angle


start : Position
start =
    Position ( 0, 0 ) 0


clockwise : Angle -> Position -> Position
clockwise degrees (Position point angle) =
    Position point (angle + degrees)


counterClockwise : Angle -> Position -> Position
counterClockwise degrees =
    clockwise (negate degrees)


advance : Int -> Position -> Position
advance amount (Position ( x, y ) angle) =
    let
        point =
            ( cos (degrees <| toFloat angle) * 20 + x
            , sin (degrees <| toFloat angle) * 20 + y
            )
    in
    Position point angle


toPathHelp :
    Production
    ->
        { current : Position
        , stack : Stack Position
        }
    -> List Svg.Path.Instruction
toPathHelp items cursor =
    case items of
        [] ->
            []

        first :: rest ->
            case first of
                'F' ->
                    let
                        nextPosition =
                            cursor.current |> advance 20

                        nextCursor =
                            { cursor | current = nextPosition }

                        (Position point _) =
                            nextPosition
                    in
                    Svg.Path.lineTo point :: toPathHelp rest nextCursor

                '+' ->
                    toPathHelp rest { cursor | current = clockwise 60 cursor.current }

                '-' ->
                    toPathHelp rest { cursor | current = counterClockwise 60 cursor.current }

                '[' ->
                    let
                        newStack =
                            cursor.stack |> push cursor.current
                    in
                    toPathHelp rest { cursor | stack = newStack }

                ']' ->
                    case pop cursor.stack of
                        Nothing ->
                            toPathHelp rest cursor

                        Just ( position, restOfTheStack ) ->
                            let
                                nextCursor =
                                    { cursor
                                        | stack = restOfTheStack
                                        , current = position
                                    }

                                (Position point _) =
                                    position
                            in
                            Svg.Path.lineTo point :: toPathHelp rest nextCursor

                _ ->
                    toPathHelp rest cursor



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


type alias Point =
    ( Float, Float )


type alias Line =
    { start : Point
    , end : Point
    , color : Color
    }


stylesheet : String
stylesheet =
    """
.Button {
    color: inherit;
    background: initial;
}

.Button:hover {
    color: white;
}
"""


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "loading..."

        Page image ->
            Html.div
                [ Html.Attributes.style
                    [ ( "height", "100%" )
                    , ( "overflow"
                      , if image.editor then
                            "hidden"
                        else
                            "initial"
                      )
                    ]
                ]
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "display", "flex" )
                        , ( "height", "100%" )
                        ]
                    ]
                    [ systemView image
                    , controlsView image
                    , Html.node "style" [] [ Html.text stylesheet ]
                    ]
                , prose
                ]


prose : Html msg
prose =
    Html.div
        [ Html.Attributes.style
            [ ( "padding", "50px" )
            , ( "max-width", "700px" )
            , ( "margin-left", "auto" )
            , ( "margin-right", "auto" )
            , ( "line-height", "1.8" )
            , ( "font-family", "-apple-system" )
            ]
        ]
        [ Html.h2 [] [ Html.text "What is an L-system?" ]
        , Html.p [] [ Html.text "An L-system or Lindenmayer system is a parallel rewriting system and a type of formal grammar. An L-system consists of an alphabet of symbols that can be used to make strings, a collection of production rules that expand each symbol into some larger string of symbols, an initial \"axiom\" string from which to begin construction, and a mechanism for translating the generated strings into geometric structures. L-systems were introduced and developed in 1968 by Aristid Lindenmayer, a Hungarian theoretical biologist and botanist at the University of Utrecht. Lindenmayer used L-systems to describe the behaviour of plant cells and to model the growth processes of plant development. L-systems have also been used to model the morphology of a variety of organisms[1] and can be used to generate self-similar fractals such as iterated function systems." ]
        , Html.h2 [] [ Html.text "Origins" ]
        , Html.p [] [ Html.text "As a biologist, Lindenmayer worked with yeast and filamentous fungi and studied the growth patterns of various types of algae, such as the cyanobacteria Anabaena catenula. Originally the L-systems were devised to provide a formal description of the development of such simple multicellular organisms, and to illustrate the neighbourhood relationships between plant cells. Later on, this system was extended to describe higher plants and complex branching structures." ]
        , Html.h2 [] [ Html.text "L-system structure" ]
        , Html.p [] [ Html.text "The recursive nature of the L-system rules leads to self-similarity and thereby, fractal-like forms are easy to describe with an L-system. Plant models and natural-looking organic forms are easy to define, as by increasing the recursion level the form slowly 'grows' and becomes more complex. Lindenmayer systems are also popular in the generation of artificial life." ]
        , Html.p [] [ Html.text "L-system grammars are very similar to the semi-Thue grammar (see Chomsky hierarchy). L-systems are now commonly known as parametric L systems, defined as a tuple" ]
        ]


controlsView : Image -> Html Msg
controlsView { system, editor } =
    Html.div
        [ Html.Attributes.style
            [ ( "background-color", colorToHex (Color.grayscale 0.9) )
            , ( "color", colorToHex (Color.grayscale 0.3) )
            , ( "padding", "40px" )
            , ( "font-family", "SFMono-Regular, monospace" )
            , ( "max-width", "500px" )
            , ( "flex-shrink", "0" )
            , ( "overflow", "auto" )
            , ( "height", "100%" )
            , ( "box-sizing", "border-box" )
            ]
        ]
        (if editor then
            [ Html.div
                [ Html.Attributes.style
                    [ ( "margin-bottom", "80px" )
                    , ( "display", "flex" )
                    , ( "align-items", "baseline" )
                    ]
                ]
                [ Html.div
                    [ Html.Attributes.style [ ( "flex", "1" ), ( "color", "white" ) ] ]
                    [ Html.text "l-system builder" ]
                , buttonTo CloseEditor "close"
                ]
            , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                [ Html.text <| "start rule: " ++ String.fromList system.start ]
            , Html.div [] [ Html.text "rules" ]
            , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                (rulesView system.rules)
            , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                [ Html.text <| "angle: " ++ (system.angle |> toString |> String.left 5) ++ " degrees" ]
            , Html.div
                [ Html.Attributes.style [ ( "text-wrap", "break-word" ), ( "margin-bottom", "40px" ) ] ]
                [ Html.text "valid characters in the rules include [ (add a new level on the stack), ] (pop a level off the stack), + (turn clockwise by given angle), - (counterclockwise), or another rule. The rules above will get expanded into:" ]
            , Html.div
                [ Html.Attributes.style [ ( "text-wrap", "break-word" ) ] ]
                [ Html.text <| String.fromList <| expand system ]
            ]
         else
            [ Html.div
                [ Html.Attributes.style
                    [ ( "color", "white" )
                    , ( "margin-bottom", "80px" )
                    , ( "transform", "rotate(90deg)" )
                    , ( "transform-origin", "center left" )
                    , ( "width", "0" )
                    , ( "line-height", "1" )
                    , ( "white-space", "pre" )
                    , ( "cursor", "pointer" )
                    ]
                , Html.Events.onClick ShowEditor
                ]
                [ Html.text "l-system builder" ]
            ]
        )


buttonTo : msg -> String -> Html msg
buttonTo msg text =
    Html.button
        [ Html.Attributes.class "Button"
        , Html.Events.onClick msg
        , Html.Attributes.style
            [ ( "border-width", "0" )
            , ( "font-family", "inherit" )
            , ( "font-size", "inherit" )
            , ( "padding", "20px 30px" )
            , ( "outline", "none" )
            , ( "cursor", "pointer" )
            ]
        ]
        [ Html.text text ]


rulesView : Rules -> List (Html msg)
rulesView rules =
    Dict.foldl ruleView [] rules


ruleView : Char -> Production -> List (Html msg) -> List (Html msg)
ruleView rule production html =
    Html.div [] [ Html.text (String.cons rule ": " ++ String.fromList production) ] :: html


systemView : Image -> Svg Msg
systemView { colorscheme, progress, system } =
    let
        styles =
            "background-color:" ++ colorToHex colorscheme.background

        path =
            expand system |> toPath system.angle |> List.singleton
    in
    Svg.svg
        [ Attributes.style styles
        , Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.preserveAspectRatio "xMidYMid meet"
        , Attributes.viewBox <| viewboxFromPath path
        , Svg.Events.on "mousemove" (Json.Decode.map ChangeProgress toProgress)
        ]
        [ pathView path colorscheme.foreground ]


viewboxFromPath : Svg.Path.Path -> String
viewboxFromPath _ =
    "-100 -300 400 500"


toProgress : Json.Decode.Decoder Float
toProgress =
    Json.Decode.map2 (/)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.at [ "target", "clientWidth" ] Json.Decode.float)


clamp : comparable -> comparable -> comparable -> comparable
clamp low high n =
    if n < low then
        low
    else if n > high then
        high
    else
        n


interpolate : Float -> Float -> Float -> Float
interpolate start end amount =
    (amount * (end - start)) + start


interpolatePoint : Point -> Point -> Float -> Point
interpolatePoint ( x1, y1 ) ( x2, y2 ) amount =
    ( interpolate x1 x2 amount, interpolate y1 y2 amount )


interpolatePoints : Float -> List Point -> List Point
interpolatePoints progress points =
    let
        multiplier =
            List.length points |> toFloat

        permitPoint ( index, point ) list =
            if toFloat index > (progress * multiplier) then
                list
            else
                point :: list
    in
    points
        |> List.indexedMap (,)
        |> List.foldl permitPoint []


interpolatePath : Float -> Svg.Path.Subpath -> Svg.Path.Subpath
interpolatePath amount path =
    path


polygon : List Point -> Svg.Path.Subpath
polygon points =
    case points of
        [] ->
            Svg.Path.emptySubpath

        first :: rest ->
            Svg.Path.subpath
                (Svg.Path.startAt first)
                Svg.Path.open
                [ Svg.Path.lineToMany rest ]


pathView : Svg.Path.Path -> Color -> Svg a
pathView path color =
    Svg.path
        [ Attributes.d <| Svg.Path.pathToStringWithPrecision 2 path
        , Attributes.fill "none"
        , Attributes.stroke <| colorToHex color
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        ]
        []
