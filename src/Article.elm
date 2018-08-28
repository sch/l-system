module Article exposing
    ( frame
    , frameWithMarkdownBody
    )

{-| This library allows you to define an article in Markdown, but have a masthead
built as an arbitrary view.

@docs frame

-}

import Html exposing (Html)
import Html.Attributes
import Html.Lazy
import Markdown


{-| The pieces of an article are:

  - whether you are focused or not on the full-screen example
  - what should go into the full-screen mast view
  - some markdown for the article itself

-}
frame : Bool -> List (Html a) -> Html a -> Html a
frame locked masthead body =
    Html.div
        [ Html.Attributes.style "height" "100%"
        , Html.Attributes.style "overflow"
            (if locked then
                "hidden"

             else
                "initial"
            )
        ]
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "height" "100%"
            ]
            masthead
        , Html.div
            [ Html.Attributes.style "padding" "50px"
            , Html.Attributes.style "max-width" "700px"
            , Html.Attributes.style "margin-left" "auto"
            , Html.Attributes.style "margin-right" "auto"
            , Html.Attributes.style "line-height" "1.8"
            , Html.Attributes.style "font-family" "-apple-system"
            ]
            [ body ]
        ]


frameWithMarkdownBody : Bool -> List (Html a) -> String -> Html a
frameWithMarkdownBody locked masthead markdownString =
    frame locked masthead <| Html.Lazy.lazy markdown markdownString


markdown : String -> Html a
markdown text =
    Markdown.toHtml [] text
