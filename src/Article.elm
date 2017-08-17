module Article exposing (frame)

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
frame : Bool -> List (Html a) -> String -> Html a
frame locked masthead prose =
    Html.div
        [ Html.Attributes.style
            [ ( "height", "100%" )
            , ( "overflow"
              , if locked then
                    "hidden"
                else
                    "initial"
              )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "height", "100%" )
                ]
            ]
            masthead
        , Html.div
            [ Html.Attributes.style
                [ ( "padding", "50px" )
                , ( "max-width", "700px" )
                , ( "margin-left", "auto" )
                , ( "margin-right", "auto" )
                , ( "line-height", "1.8" )
                , ( "font-family", "-apple-system" )
                ]
            ]
            [ Html.Lazy.lazy markdown prose ]
        ]


markdown : String -> Html a
markdown text =
    Markdown.toHtml [] text
