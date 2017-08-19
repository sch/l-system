module Pointer exposing (Offset, Percentage, horizontal, progress, vertical)

{-| Grab useful bits of information off the DOM.


# Mouse-related stuff

**Types**

@docs Amount, Progress

**Functions**

@docs mouse, mouseHorizontal, mouseVertical

-}

import Json.Decode as Json exposing (Decoder)


-- Mouse Movement


{-| A float between 0 and 1 representing how far from an edge the mouse is.
-}
type alias Percentage =
    Float


{-| The horizontal and vertical components representing how far from the top
and left edges the mouse is over the given element.
-}
type alias Offset =
    { horizontal : Percentage
    , vertical : Percentage
    }


{-| Trigger a message indicating the horizontal and vertical component of how
far the mouse has traveled over the element.
-}
progress : (Offset -> msg) -> Decoder msg
progress msg =
    Json.map2 Offset toHorizontalProgress toVerticalProgress
        |> Json.map msg


{-| Trigger a message indicating the horizontal component of how far the mouse
has traveled over the element.
-}
horizontal : (Percentage -> msg) -> Decoder msg
horizontal msg =
    Json.map msg toHorizontalProgress


{-| Trigger a message indicating the vertical component of how far the mouse
has traveled over the element.
-}
vertical : (Percentage -> msg) -> Decoder msg
vertical msg =
    Json.map msg toVerticalProgress


toHorizontalProgress : Decoder Percentage
toHorizontalProgress =
    let
        decodeMouseX =
            Json.field "clientX" Json.float

        decodeWidth =
            Json.at [ "target", "clientWidth" ] Json.float
                |> Json.andThen (failIfZero "Can't support a width of zero")
    in
    Json.map2 (/) decodeMouseX decodeWidth


toVerticalProgress : Decoder Percentage
toVerticalProgress =
    let
        decodeMouseY =
            Json.field "clientY" Json.float

        decodeHeight =
            Json.at [ "target", "clientHeight" ] Json.float
                |> Json.andThen (failIfZero "Can't support a height of zero")
    in
    Json.map2 (/) decodeMouseY decodeHeight


failIfZero : String -> Float -> Decoder Float
failIfZero message value =
    if value == 0 then
        Json.fail message
    else
        Json.succeed value
