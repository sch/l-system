module Main exposing (main)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html exposing (Html)
import Mouse
import Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Task
import Window


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Loading
    | Page Image


type alias Image =
    { colorscheme : Colorscheme
    , progress : Float
    , canvas : { width : Int }
    }


type alias Colorscheme =
    { background : Color, foreground : Color }


type Msg
    = Generate
    | Build Color
    | MoveMouse Mouse.Position
    | Resize Window.Size


init : ( Model, Cmd Msg )
init =
    let
        loadCommand =
            Task.perform (always Generate) (Task.succeed 0)
    in
        ( Loading, loadCommand )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate Build randomColor )

        Build color ->
            let
                image =
                    { colorscheme = toColorscheme color
                    , progress = 1
                    , canvas = { width = 100 }
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading ->
            Sub.none

        Page _ ->
            Sub.batch
                [ Mouse.moves MoveMouse
                , Window.resizes Resize
                ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text "loading..."

        Page system ->
            systemView system


systemView : Image -> Svg a
systemView { colorscheme, progress } =
    let
        styles =
            "background-color:" ++ colorToHex colorscheme.background
    in
        Svg.svg
            [ Attributes.style styles
            , Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.preserveAspectRatio "xMidYMid meet"
            , Attributes.viewBox "0 0 1 1"
            ]
            (lines colorscheme.foreground progress)


lines : Color -> Float -> List (Svg a)
lines color progress =
    [ Svg.line
        [ Attributes.x1 "0"
        , Attributes.y1 "0"
        , Attributes.x2 (toString progress)
        , Attributes.y2 (toString progress)
        , Attributes.strokeWidth "0.01"
        , Attributes.stroke (colorToHex color)
        ]
        []
    ]
