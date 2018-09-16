module Colorscheme exposing (Colorscheme, complementary, random)

import Color exposing (Color)
import Random
import Random.Color as Random


type alias Colorscheme =
    { background : Color, foreground : Color }


{-| Builds a complementary background color for the given foreground color,
ensuring that there's enough contrast between the two.
-}
complementary : Color -> Colorscheme
complementary color =
    let
        { hue, saturation, lightness } =
            Color.toHsla color

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


{-| Generator for a random, pleasing, high-contrast colorscheme.
-}
random : Random.Generator Colorscheme
random =
    Random.color |> Random.map complementary
