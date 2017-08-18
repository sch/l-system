module Controls exposing (Config, config, dict, int, string, text, view)

import Color
import Color.Convert exposing (colorToHex)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events


-- Config


type Config msg
    = Config
        { openControls : msg
        , hideControls : msg
        , title : String
        }


config :
    { onOpen : msg
    , onDismiss : msg
    , title : String
    }
    -> Config msg
config { onOpen, onDismiss, title } =
    Config
        { openControls = onOpen
        , hideControls = onDismiss
        , title = title
        }



-- State


type alias State =
    { visible : Bool }


state : State
state =
    { visible = False }



-- View


stylesheet : String
stylesheet =
    """
.Button {
    color: inherit;
    background: initial;
}

.Button:hover {
    color: white;
}
"""


view : Config msg -> State -> List (Html msg) -> Html msg
view config state body =
    let
        (Config { openControls, hideControls, title }) =
            config

        styles =
            [ ( "background-color", colorToHex (Color.grayscale 0.9) )
            , ( "color", colorToHex (Color.grayscale 0.3) )
            , ( "padding", "40px" )
            , ( "font-family", "SFMono-Regular, monospace" )
            , ( "max-width", "500px" )
            , ( "flex-shrink", "0" )
            , ( "overflow", "auto" )
            , ( "height", "100%" )
            , ( "box-sizing", "border-box" )
            ]

        styleTag =
            Html.node "style" [] [ Html.text stylesheet ]

        attributes =
            if state.visible then
                [ Html.Attributes.style styles
                ]
            else
                [ Html.Attributes.style <| ( "cursor", "pointer" ) :: styles
                , Html.Events.onClick openControls
                ]

        body_ =
            if state.visible then
                [ styleTag, heading hideControls title ] ++ body
            else
                [ styleTag, sidewaysTitle title ]
    in
    Html.div attributes body_


heading : msg -> String -> Html msg
heading closeMessage title =
    Html.div
        [ Html.Attributes.style
            [ ( "margin-bottom", "80px" )
            , ( "display", "flex" )
            , ( "align-items", "baseline" )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style [ ( "flex", "1" ), ( "color", "white" ) ] ]
            [ Html.text title ]
        , buttonTo closeMessage "close"
        ]


sidewaysTitle : String -> Html msg
sidewaysTitle title =
    Html.div
        [ Html.Attributes.style
            [ ( "color", "white" )
            , ( "margin-bottom", "80px" )
            , ( "transform", "rotate(90deg)" )
            , ( "transform-origin", "center left" )
            , ( "width", "0" )
            , ( "line-height", "1" )
            , ( "white-space", "pre" )
            ]
        ]
        [ Html.text title ]


label : String -> Html msg -> Html msg
label text element =
    Html.div
        [ Html.Attributes.style [ ( "margin-bottom", "40px" ) ] ]
        [ Html.text (text ++ ": "), element ]


string : String -> String -> Html msg
string labelText value =
    label labelText (Html.text value)


int : String -> int -> Html msg
int labelText value =
    label labelText <| Html.text (toString value)


dict : String -> Dict String String -> Html msg
dict labelText dict =
    label labelText <|
        Html.div
            [ Html.Attributes.style [ ( "padding-left", "20px" ) ] ]
            (Dict.foldl keyValueView [] dict)


keyValueView : String -> String -> List (Html msg) -> List (Html msg)
keyValueView key value html =
    Html.div [] [ Html.text (key ++ ": " ++ value) ] :: html


text : String -> Html msg
text text =
    Html.div
        [ Html.Attributes.style
            [ ( "word-wrap", "break-word" )
            , ( "overflow-wrap", "break-word" )
            , ( "margin-bottom", "40px" )
            ]
        ]
        [ Html.text text ]


buttonTo : msg -> String -> Html msg
buttonTo msg text =
    Html.button
        [ Html.Attributes.class "Button"
        , Html.Events.onClick msg
        , Html.Attributes.style
            [ ( "border-width", "0" )
            , ( "font-family", "inherit" )
            , ( "font-size", "inherit" )
            , ( "padding", "20px 30px" )
            , ( "outline", "none" )
            , ( "cursor", "pointer" )
            ]
        ]
        [ Html.text text ]
