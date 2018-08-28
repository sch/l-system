module Color exposing (Color(..), colorToHex, grayscale, hsl, toHsl)


type Color
    = RGBA Int Int Int Float
    | HSLA Float Float Float Float


grayscale : Float -> Color
grayscale p =
    HSLA 0 0 (1 - p) 1


fmod : Float -> Int -> Float
fmod f n =
    let
        integer =
            floor f
    in
    toFloat (integer |> remainderBy n) + f - toFloat integer


hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
    HSLA (hue - turns (toFloat (floor (hue / (2 * pi))))) saturation lightness alpha


hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
    hsla hue saturation lightness 1


hslToRgb : Float -> Float -> Float -> ( Float, Float, Float )
hslToRgb hue saturation lightness =
    let
        chroma =
            (1 - abs (2 * lightness - 1)) * saturation

        normHue =
            hue / degrees 60

        x =
            chroma * (1 - abs (fmod normHue 2 - 1))

        ( r, g, b ) =
            if normHue < 0 then
                ( 0, 0, 0 )

            else if normHue < 1 then
                ( chroma, x, 0 )

            else if normHue < 2 then
                ( x, chroma, 0 )

            else if normHue < 3 then
                ( 0, chroma, x )

            else if normHue < 4 then
                ( 0, x, chroma )

            else if normHue < 5 then
                ( x, 0, chroma )

            else if normHue < 6 then
                ( chroma, 0, x )

            else
                ( 0, 0, 0 )

        m =
            lightness - chroma / 2
    in
    ( r + m, g + m, b + m )


toRgb : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
toRgb color =
    case color of
        RGBA r g b a ->
            { red = r, green = g, blue = b, alpha = a }

        HSLA h s l a ->
            let
                ( r, g, b ) =
                    hslToRgb h s l
            in
            { red = round (255 * r)
            , green = round (255 * g)
            , blue = round (255 * b)
            , alpha = a
            }


toHsl : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsl color =
    case color of
        HSLA h s l a ->
            { hue = h, saturation = s, lightness = l, alpha = a }

        RGBA r g b a ->
            let
                ( h, s, l ) =
                    rgbToHsl r g b
            in
            { hue = h, saturation = s, lightness = l, alpha = a }


rgbToHsl : Int -> Int -> Int -> ( Float, Float, Float )
rgbToHsl redAmount greenAmount blueAmount =
    let
        r =
            toFloat redAmount / 255

        g =
            toFloat greenAmount / 255

        b =
            toFloat blueAmount / 255

        cMax =
            max (max r g) b

        cMin =
            min (min r g) b

        c =
            cMax - cMin

        hue =
            degrees 60
                * (if cMax == r then
                    fmod ((g - b) / c) 6

                   else if cMax == g then
                    ((b - r) / c) + 2

                   else
                    {- cMax == b -}
                    ((r - g) / c) + 4
                  )

        lightness =
            (cMax + cMin) / 2

        saturation =
            if lightness == 0 then
                0

            else
                c / (1 - abs (2 * lightness - 1))
    in
    ( hue, saturation, lightness )


toHex : Int -> String
toHex =
    toRadix >> String.padLeft 2 '0'


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                String.fromInt c

            else
                String.fromChar <| Char.fromCode (87 + c)
    in
    if n < 16 then
        getChr n

    else
        toRadix (n // 16) ++ getChr (modBy 16 n)


colorToHex : Color -> String
colorToHex color =
    let
        { red, green, blue } =
            toRgb color
    in
    List.map toHex [ red, green, blue ]
        |> (::) "#"
        |> String.join ""
