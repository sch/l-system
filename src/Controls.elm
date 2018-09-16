module Controls exposing
    ( Config
    , State
    , config
    , dict
    , hide
    , int
    , show
    , state
    , string
    , text
    , union
    , view
    )

import Color exposing (Color)
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
    { visible = True }


show : State -> State
show state_ =
    { state_ | visible = True }


hide : State -> State
hide state_ =
    { state_ | visible = False }


type alias RadioButtonState option =
    { label : String
    , option : option
    , isSelected : Bool
    }



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
    transition-duration: 0.2s;
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
view (Config { openControls, hideControls, title }) state_ controls =
    let
        styles =
            [ Html.Attributes.style "background-color" <| Color.toCssString Color.darkCharcoal
            , Html.Attributes.style "color" <| Color.toCssString Color.darkGrey
            , Html.Attributes.style "font-family" "SFMono-Regular, 'Inconsolata', monospace"
            , Html.Attributes.style "flex-shrink" "0"
            , Html.Attributes.style "overflow" "auto"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "box-sizing" "border-box"
            ]

        styleTag =
            Html.node "style" [] [ Html.text stylesheet ]

        attributes =
            if state.visible then
                styles

            else
                [ Html.Attributes.style "cursor" "pointer"
                , Html.Events.onClick openControls
                ]
                    |> List.append styles

        body =
            if state.visible then
                [ styleTag, heading hideControls title ]
                    ++ [ Html.div
                            [ Html.Attributes.style "display" "table"
                            , Html.Attributes.style "padding" "0 40px"
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
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "padding" "0 0 40px 40px"
        , Html.Attributes.style "align-items" "baseline"
        ]
        [ Html.div
            [ Html.Attributes.style "flex" "1", Html.Attributes.style "color" "white" ]
            [ Html.text title ]
        , buttonTo closeMessage "close"
        ]


sidewaysTitle : String -> Html msg
sidewaysTitle title =
    Html.div
        [ Html.Attributes.style "color" "white"
        , Html.Attributes.style "writing-mode" "vertical-lr"
        , Html.Attributes.style "margin" "40px"
        ]
        [ Html.text title ]


label : Label -> Html msg -> Html msg
label labelText element =
    let
        leftHand =
            Html.div
                [ Html.Attributes.style "white-space" "pre"
                , Html.Attributes.style "display" "table-cell"
                , Html.Attributes.style "text-align" "right"
                ]
                [ Html.text labelText ]

        rightHand =
            Html.div
                [ Html.Attributes.style "padding-bottom" "40px"
                , Html.Attributes.style "padding-left" "20px"
                , Html.Attributes.style "display" "table-cell"
                , Html.Attributes.style "white-space" "pre"
                ]
                [ element ]
    in
    Html.div
        [ Html.Attributes.style "display" "table-row-group" ]
        [ Html.div
            [ Html.Attributes.style "display" "table-row"
            , Html.Attributes.style "vertical-align" "center"
            ]
            [ leftHand, rightHand ]
        ]


string : Label -> String -> Html msg
string text_ value =
    label text_ (Html.text value)


int : Label -> Int -> (String -> msg) -> Html msg
int labelText value handleChange =
    let
        input =
            Html.input
                [ Html.Attributes.value (String.fromInt value)
                , Html.Attributes.type_ "number"
                , Html.Attributes.class "Input"
                , Html.Attributes.size 5
                , Html.Attributes.min "0"
                , Html.Attributes.pattern "[0-9]*"
                , Html.Events.onInput handleChange
                , Html.Attributes.style "font-family" "inherit"
                , Html.Attributes.style "font-size" "inherit"
                , Html.Attributes.style "border-radius" "0.4em"
                , Html.Attributes.style "outline" "none"
                , Html.Attributes.style "width" "3em"
                ]
                []
    in
    label labelText input


union : Label -> List ( String, choice ) -> choice -> (choice -> msg) -> Html msg
union labelText choices selected handleSelect =
    choices
        |> List.map (indicateWhetherSelected selected)
        |> List.map (radioButton handleSelect)
        |> Html.fieldset []
        |> label labelText


indicateWhetherSelected : choice -> ( String, choice ) -> RadioButtonState choice
indicateWhetherSelected selected ( labelText, choice ) =
    { label = labelText
    , option = choice
    , isSelected = choice == selected
    }


radioButton : (option -> msg) -> RadioButtonState option -> Html msg
radioButton handleSelect option =
    Html.label
        [ Html.Attributes.style "display" "block"
        , Html.Attributes.style "padding-bottom" "20px"
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.input
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.style "margin-right" "10px"
            , Html.Events.onClick (handleSelect option.option)
            , Html.Attributes.name "option"
            , Html.Attributes.checked option.isSelected
            ]
            []
        , Html.text option.label
        ]


dict : String -> Dict String String -> Html msg
dict labelText dict_ =
    label labelText <|
        Html.div
            []
            (Dict.foldl keyValueView [] dict_)


keyValueView : String -> String -> List (Html msg) -> List (Html msg)
keyValueView key value html =
    Html.div [] [ Html.text (key ++ ": " ++ value) ] :: html


text : String -> Html msg
text text_ =
    Html.div
        [ Html.Attributes.style "word-wrap" "break-word"
        , Html.Attributes.style "overflow-wrap" "break-word"
        , Html.Attributes.style "display" "table-caption"
        , Html.Attributes.style "caption-side" "bottom"
        , Html.Attributes.style "padding" "40px"
        ]
        [ Html.text text_ ]


buttonTo : msg -> String -> Html msg
buttonTo msg buttonText =
    Html.button
        [ Html.Attributes.class "Button"
        , Html.Events.onClick msg
        , Html.Attributes.style "border-width" "0"
        , Html.Attributes.style "font-family" "inherit"
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "padding" "40px 40px"
        , Html.Attributes.style "outline" "none"
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.text buttonText ]
