module System exposing (Config, System, config, expand, view)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Colorscheme exposing (Colorscheme)
import Dict exposing (Dict)
import Dom
import Stack exposing (Stack)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Path


-- Config


type Config msg
    = Config { reportPosition : Float -> msg }


config : { onSwipe : Float -> msg } -> Config msg
config { onSwipe } =
    Config { reportPosition = onSwipe }


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
    | Rule Char


type alias Point =
    ( Float, Float )


type alias Extent =
    { min : Point
    , max : Point
    }


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
                    |> Maybe.withDefault [ char ]
        in
        expand
            { system
                | start = List.concatMap expansionFor system.start
                , iterations = system.iterations - 1
            }


type Position
    = Position Point Angle


type alias State =
    { current : Position
    , stack : Stack Position
    , size : Extent
    }


start : Position
start =
    Position ( 0, 0 ) 0


clockwise : Angle -> Position -> Position
clockwise degrees (Position point angle) =
    Position point (angle + degrees)


counterClockwise : Angle -> Position -> Position
counterClockwise degrees =
    clockwise (negate degrees)


advance : Float -> Position -> Position
advance amount (Position ( x, y ) angle) =
    let
        point =
            ( cos (degrees <| toFloat angle) * amount + x
            , sin (degrees <| toFloat angle) * amount + y
            )
    in
    Position point angle


grow : Point -> Extent -> Extent
grow point extent =
    Extent (pointMin extent.min point) (pointMax extent.max point)


pointMin : Point -> Point -> Point
pointMin ( x1, y1 ) ( x2, y2 ) =
    ( min x1 x2, min y1 y2 )


pointMax : Point -> Point -> Point
pointMax ( x1, y1 ) ( x2, y2 ) =
    ( max x1 x2, max y1 y2 )


toSubpath : List Point -> Svg.Path.Subpath
toSubpath points =
    case points of
        [] ->
            Svg.Path.emptySubpath

        head :: rest ->
            Svg.Path.subpath (Svg.Path.startAt head)
                Svg.Path.open
                [ Svg.Path.lineToMany rest ]


toPath : Int -> Float -> Production -> ( Svg.Path.Path, Extent )
toPath angle length chars =
    let
        startState =
            State start Stack.empty (Extent ( 0, 0 ) ( 0, 0 ))

        toPathHelp : Production -> State -> List Point -> Svg.Path.Path -> ( Svg.Path.Path, Extent )
        toPathHelp chars cursor points path =
            case chars of
                [] ->
                    ( toSubpath points :: path, cursor.size )

                first :: rest ->
                    case first of
                        'F' ->
                            let
                                nextPosition =
                                    cursor.current |> advance length

                                (Position point _) =
                                    nextPosition

                                nextSize =
                                    grow point cursor.size

                                nextCursor =
                                    { cursor
                                        | current = nextPosition
                                        , size = nextSize
                                    }

                                newPoints =
                                    point :: points
                            in
                            toPathHelp rest nextCursor newPoints path

                        '+' ->
                            let
                                nextCursor =
                                    { cursor | current = counterClockwise angle cursor.current }
                            in
                            toPathHelp rest nextCursor points path

                        '-' ->
                            let
                                nextCursor =
                                    { cursor | current = clockwise angle cursor.current }
                            in
                            toPathHelp rest nextCursor points path

                        '[' ->
                            let
                                newStack =
                                    cursor.stack |> Stack.push cursor.current

                                nextCursor =
                                    { cursor | stack = newStack }
                            in
                            toPathHelp rest nextCursor points path

                        ']' ->
                            case Stack.pop cursor.stack of
                                Nothing ->
                                    toPathHelp rest cursor points path

                                Just ( position, restOfTheStack ) ->
                                    let
                                        (Position point _) =
                                            position

                                        nextCursor =
                                            { cursor
                                                | stack = restOfTheStack
                                                , current = position
                                                , size = grow point cursor.size
                                            }

                                        newPoints =
                                            [ point ]

                                        newPath =
                                            toSubpath points :: path
                                    in
                                    toPathHelp rest nextCursor newPoints newPath

                        _ ->
                            toPathHelp rest cursor points path
    in
    toPathHelp chars startState [ ( 0, 0 ) ] []



-- View


view : Config msg -> Colorscheme -> System -> Svg msg
view (Config { reportPosition }) colorscheme system =
    let
        _ =
            Debug.log "rendering system" True

        styles =
            "background-color:" ++ colorToHex colorscheme.background

        ( path, extent ) =
            expand system |> toPath system.angle 10
    in
    Svg.svg
        [ Attributes.style styles
        , Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.preserveAspectRatio "xMidYMid meet"
        , Attributes.viewBox <| viewboxString extent
        , Dom.mouseHorizontal reportPosition
        ]
        [ pathView path colorscheme.foreground ]


viewboxString : Extent -> String
viewboxString { min, max } =
    let
        padding =
            1.2

        ( minX, minY ) =
            min

        ( maxX, maxY ) =
            max

        x =
            minX * padding

        y =
            minY * padding

        width =
            (maxX - x) * padding

        height =
            (maxY - y) * padding
    in
    [ x, y, width, height ]
        |> List.map toString
        |> String.join " "


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


pathView : Svg.Path.Path -> Color -> Svg a
pathView path color =
    Svg.path
        [ Attributes.d <| Svg.Path.pathToString path
        , Attributes.fill "none"
        , Attributes.stroke <| colorToHex color
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        ]
        []
