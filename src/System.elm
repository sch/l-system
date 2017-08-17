module System exposing (System, expand, view)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Colorscheme exposing (Colorscheme)
import Dict exposing (Dict)
import Dom
import Stack exposing (Stack)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Path


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


type alias Point =
    ( Float, Float )


type alias Line =
    { start : Point
    , end : Point
    , color : Color
    }


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


toPath : Int -> Production -> Svg.Path.Subpath
toPath angle items =
    Svg.Path.subpath
        (Svg.Path.startAt ( 0, 0 ))
        Svg.Path.open
        (toPathHelp items { current = start, stack = [] })


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
                            cursor.stack |> Stack.push cursor.current
                    in
                    toPathHelp rest { cursor | stack = newStack }

                ']' ->
                    case Stack.pop cursor.stack of
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


view : Colorscheme -> Float -> (Float -> msg) -> System -> Svg msg
view colorscheme progress msg system =
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
        , Dom.mouseHorizontal msg
        ]
        [ pathView path colorscheme.foreground ]


viewboxFromPath : Svg.Path.Path -> String
viewboxFromPath _ =
    "-100 -300 400 500"


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
