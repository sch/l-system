module Dom exposing (Amount, Progress, mouse, mouseHorizontal)

{-| Grab useful bits of information off the DOM.


# Mouse-related stuff

**Types**

@docs Amount, Progress

**Functions**

@docs mouse, mouseHorizontal, mouseVertical

-}

import Html exposing (Attribute, Html)
import Html.Events
import Json.Decode exposing (Decoder)


-- Mouse Movement


{-| A float between 0 and 1 representing how far from an edge the mouse is.
-}
type alias Amount =
    Float


{-| The horizontal and vertical components representing how far from the top
and left edges the mouse is over the given element.
-}
type alias Progress =
    { horizontal : Float
    , vertical : Float
    }


{-| Trigger a message indicating the horizontal and vertical component of how
far the mouse has traveled over the element.
-}
mouse : (Progress -> msg) -> Attribute msg
mouse msg =
    Html.Events.on "mousemove" <| Json.Decode.map msg toProgress


{-| Trigger a message indicating the horizontal component of how far the mouse
has traveled over the element.
-}
mouseHorizontal : (Float -> msg) -> Attribute msg
mouseHorizontal msg =
    Html.Events.on "mousemove" <| Json.Decode.map msg toHorizontalProgress


{-| Trigger a message indicating the vertical component of how far the mouse
has traveled over the element.
-}
mouseVertical : (Float -> msg) -> Attribute msg
mouseVertical msg =
    Html.Events.on "mousemove" <| Json.Decode.map msg toVerticalProgress


toProgress : Json.Decode.Decoder Progress
toProgress =
    Json.Decode.map2 Progress toHorizontalProgress toVerticalProgress


toHorizontalProgress : Json.Decode.Decoder Float
toHorizontalProgress =
    Json.Decode.map2 (/)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.at [ "target", "clientWidth" ] Json.Decode.float)


toVerticalProgress : Json.Decode.Decoder Float
toVerticalProgress =
    Json.Decode.map2 (/)
        (Json.Decode.field "clientY" Json.Decode.float)
        (Json.Decode.at [ "target", "clientHeight" ] Json.Decode.float)
