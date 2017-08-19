module Controls exposing (Config, State, config, dict, hide, int, show, state, string, text, union, view)

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


show : State -> State
show state =
    { state | visible = True }


hide : State -> State
hide state =
    { state | visible = False }



-- View


type alias Label =
    String


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

.Input {
    background-color: inherit;
    border-style: solid;
    border-width: 2px;
    border-bottom-color: inherit;
    border-right-color: inherit;
    padding: 0.4em 0.8em;
    color: inherit;
    transition-duration: 0.15s;
    transition-properties: border-color, color;
    background-color 0.1s ease-in-out;
}

.Input:focus {
    border-color: white;
    color: white;
}

fieldset {
    border: none;
    padding: 0;
}
"""


view : Config msg -> State -> List (Html msg) -> Html msg
view config state controls =
    let
        (Config { openControls, hideControls, title }) =
            config

        styles =
            [ ( "background-color", colorToHex (Color.grayscale 0.9) )
            , ( "color", colorToHex (Color.grayscale 0.3) )
            , ( "font-family", "SFMono-Regular, monospace" )
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

        body =
            if state.visible then
                [ styleTag, heading hideControls title ]
                    ++ [ Html.div
                            [ Html.Attributes.style
                                [ ( "display", "table" )
                                , ( "padding", "0 40px" )
                                ]
                            ]
                            controls
                       ]
            else
                [ styleTag, sidewaysTitle title ]
    in
    Html.div attributes body


heading : msg -> String -> Html msg
heading closeMessage title =
    Html.div
        [ Html.Attributes.style
            [ ( "display", "flex" )
            , ( "padding", "0 0 40px 40px" )
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
            , ( "margin", "40px" )
            , ( "width", "0" )
            , ( "line-height", "1" )
            , ( "white-space", "pre" )
            ]
        ]
        [ Html.text title ]


label : Label -> Html msg -> Html msg
label text element =
    let
        leftHand =
            Html.div
                [ Html.Attributes.style
                    [ ( "white-space", "pre" )
                    , ( "display", "table-cell" )
                    , ( "text-align", "right" )
                    ]
                ]
                [ Html.text text ]

        rightHand =
            Html.div
                [ Html.Attributes.style
                    [ ( "padding-bottom", "40px" )
                    , ( "padding-left", "20px" )
                    , ( "display", "table-cell" )
                    , ( "white-space", "pre" )
                    ]
                ]
                [ element ]
    in
    Html.div
        [ Html.Attributes.style [ ( "display", "table-row-group" ) ] ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "display", "table-row" )
                , ( "vertical-align", "center" )
                ]
            ]
            [ leftHand, rightHand ]
        ]


string : Label -> String -> Html msg
string text value =
    label text (Html.text value)


int : Label -> int -> (String -> msg) -> Html msg
int text value handleChange =
    let
        input =
            Html.input
                [ Html.Attributes.value (toString value)
                , Html.Attributes.type_ "number"
                , Html.Attributes.class "Input"
                , Html.Attributes.size 5
                , Html.Events.onInput handleChange
                , Html.Attributes.style
                    [ ( "font-family", "inherit" )
                    , ( "font-size", "inherit" )
                    , ( "border-radius", "0.4em" )
                    , ( "outline", "none" )
                    , ( "width", "3em" )
                    ]
                ]
                []
    in
    label text input


union : Label -> List ( String, choice ) -> (choice -> msg) -> Html msg
union text choices handleSelect =
    label text <| Html.fieldset [] <| List.map (radioButton handleSelect) choices


radioButton : (option -> msg) -> ( String, option ) -> Html msg
radioButton handleSelect ( label, option ) =
    Html.label
        [ Html.Attributes.style
            [ ( "display", "block" )
            , ( "padding-bottom", "20px" )
            , ( "cursor", "pointer" )
            ]
        ]
        [ Html.input
            [ Html.Attributes.type_ "radio"
            , Html.Events.onClick (handleSelect option)
            ]
            []
        , Html.text label
        ]


dict : String -> Dict String String -> Html msg
dict labelText dict =
    label labelText <|
        Html.div
            []
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
            , ( "display", "table-caption" )
            , ( "caption-side", "bottom" )
            , ( "padding", "40px" )
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
            , ( "padding", "40px 40px" )
            , ( "outline", "none" )
            , ( "cursor", "pointer" )
            ]
        ]
        [ Html.text text ]
