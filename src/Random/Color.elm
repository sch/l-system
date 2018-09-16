module Random.Color exposing (color)

import Color exposing (Color)
import Random


{-| Generator for a random pleasing color.
-}
color : Random.Generator Color
color =
    let
        hue =
            Random.float 0 1

        saturation =
            Random.float 0.1 0.9

        lightness =
            Random.float 0.1 0.9
    in
    Random.map3 Color.hsl hue saturation lightness
