module Main exposing (main)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Mouse
import Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Path
import Task
import Window


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


type alias Radian =
    Float


type alias Image =
    { colorscheme : Colorscheme
    , progress : Float
    , iterations : Int
    , angle : Int
    , canvas : { width : Int }
    , system : System
    , editor : Bool
    }


type alias System =
    { start : String
    , rules : Dict String String
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
    = Generate
    | Build Color
    | ToggleControls
    | MoveMouse Mouse.Position
    | Resize Window.Size


init : ( Model, Cmd Msg )
init =
    let
        loadCommand =
            Task.succeed Generate |> Task.perform identity
    in
        ( Loading, loadCommand )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate Build randomColor )

        Build color ->
            let
                system =
                    { start = "NSH"
                    , rules =
                        Dict.fromList
                            [ ( "F", "H-[F[-]]" )
                            , ( "D", "NNHF" )
                            , ( "N", "[]S-HFSHF" )
                            , ( "H", "[-[[N]DS-]]" )
                            , ( "S", "[N-+SHDN]" )
                            ]
                    }

                image =
                    { colorscheme = toColorscheme color
                    , progress = 1
                    , iterations = 4
                    , angle = 90
                    , system = system
                    , canvas = { width = 100 }
                    , editor = False
                    }
            in
                ( Page image, Cmd.none )

        MoveMouse position ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    let
                        progress =
                            toFloat position.x / toFloat image.canvas.width

                        page =
                            Page { image | progress = progress }

                        command =
                            Task.perform Resize Window.size
                    in
                        ( page, command )

        Resize { width } ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    ( Page { image | canvas = { width = width } }, Cmd.none )

        ToggleControls ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Page image ->
                    ( Page { image | editor = not image.editor }, Cmd.none )


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



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Page model ->
            Sub.batch
                [ Mouse.moves MoveMouse
                , Window.resizes Resize
                ]



-- View


type alias Point =
    ( Float, Float )


type alias Line =
    { start : Point
    , end : Point
    , color : Color
    }


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "loading..."

        Page system ->
            Html.div
                [ Html.Attributes.style
                    [ ( "display", "flex" )
                    , ( "height", "100%" )
                    ]
                ]
                [ systemView system
                , controlsView system
                ]


controlsView : Image -> Html Msg
controlsView { iterations, angle, system, editor } =
    Html.div
        [ Html.Attributes.style
            [ ( "background-color", colorToHex (Color.grayscale 0.9) )
            , ( "color", colorToHex (Color.grayscale 0.3) )
            , ( "padding", "40px" )
            , ( "font-family", "SFMono-Regular, monospace" )
            , ( "max-width", "400px" )
            , ( "flex-shrink", "0" )
            ]
        ]
        (if editor then
            ([ Html.div
                [ Html.Attributes.style [ ( "color", "white" ), ( "margin-bottom", "80px" ) ] ]
                [ Html.text "l-system builder" ]
             , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                [ Html.text <| "start rule: " ++ system.start ]
             , Html.div [] [ Html.text "rules" ]
             , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                (rulesView system.rules)
             , Html.div
                [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
                [ Html.text <| "angle: " ++ (angle |> toString |> String.left 5) ++ " degrees" ]
             , Html.div
                [ Html.Attributes.style [ ( "text-wrap", "break-word" ) ] ]
                [ Html.text "valid characters in the rules include [ (add a new level on the stack), ] (pop a level off the stack), + (turn clockwise by given angle), - (counterclockwise), or another rule. " ]
             ]
            )
         else
            ([ Html.div
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
                , Html.Events.onClick ToggleControls
                ]
                [ Html.text "l-system builder" ]
             ]
            )
        )


rulesView : Dict String String -> List (Html msg)
rulesView rules =
    Dict.foldl ruleView [] rules


ruleView : String -> String -> List (Html msg) -> List (Html msg)
ruleView rule production html =
    Html.div [] [ Html.text (rule ++ ": " ++ production) ] :: html


lines : Color -> List Line
lines color =
    [ Line ( 0, 0 ) ( 0, 0 ) color
    , Line ( 0, 0 ) ( 0.25, 0.25 ) color
    , Line ( 0.25, 0.25 ) ( 0.75, 0.25 ) color
    , Line ( 0.75, 0.25 ) ( 0.75, 0.75 ) color
    , Line ( 0.75, 0.75 ) ( 0.25, 0.75 ) color
    , Line ( 0.25, 0.75 ) ( 0.25, 0.25 ) color
    , Line ( 0.25, 0.25 ) ( 0, 0 ) color
    , Line ( 0, 0 ) ( 0, 1 ) color
    , Line ( 0, 1 ) ( 1, 1 ) color
    , Line ( 1, 1 ) ( 1, 0 ) color
    , Line ( 1, 0 ) ( 0, 0 ) color
    , Line ( 0, 0 ) ( 0, 0 ) color
    ]


systemView : Image -> Svg a
systemView { colorscheme, progress } =
    let
        styles =
            "background-color:" ++ colorToHex colorscheme.background

        points =
            [ ( 0, 0 )
            , ( 0.25, 0.25 )
            , ( 0.75, 0.25 )
            , ( 0.75, 0.75 )
            , ( 0.25, 0.75 )
            , ( 0.25, 0.25 )
            , ( 0, 0 )
            , ( 0, 1 )
            , ( 1, 1 )
            , ( 1, 0 )
            , ( 0, 0 )
            ]
                |> interpolatePoints progress
    in
        Svg.svg
            [ Attributes.style styles
            , Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.preserveAspectRatio "xMidYMid meet"
            , Attributes.viewBox "-0.1 -0.1 1.1 1.2"
            ]
            [ path points colorscheme.foreground ]


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


path : List Point -> Color -> Svg a
path points color =
    let
        polygon =
            case points of
                [] ->
                    Svg.Path.emptySubpath

                first :: rest ->
                    Svg.Path.subpath
                        (Svg.Path.startAt first)
                        Svg.Path.open
                        [ Svg.Path.lineToMany rest ]
    in
        Svg.path
            [ Attributes.d <| Svg.Path.pathToString [ polygon ]
            , Attributes.fill "none"
            , Attributes.stroke <| colorToHex color
            , Attributes.strokeWidth "0.02"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []