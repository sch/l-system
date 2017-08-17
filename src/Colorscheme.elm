module Colorscheme exposing (Colorscheme, complementary)

import Color exposing (Color)


type alias Colorscheme =
    { background : Color, foreground : Color }


{-| Builds a complementary background color for the given foreground color,
ensuring that there's enough contrast between the two.
-}
complementary : Color -> Colorscheme
complementary color =
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
