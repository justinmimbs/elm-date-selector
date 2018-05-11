module Dropdown exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed


view : msg -> Html msg -> Maybe (Html msg) -> Html msg
view close button maybeContent =
    let
        buttonContainer =
            div
                [ class "dropdown--button-container" ]
                [ button ]
    in
    case maybeContent of
        Nothing ->
            Html.Keyed.node "div"
                [ class "dropdown" ]
                [ ( "button"
                  , buttonContainer
                  )
                ]

        Just content ->
            Html.Keyed.node "div"
                [ class "dropdown-open" ]
                [ ( "cover"
                  , div
                        [ class "dropdown--page-cover"
                        , onClick close
                        , Html.Attributes.style "background" "rgba(0, 0, 0, 0.1)"
                        ]
                        []
                  )
                , ( "button"
                  , buttonContainer
                  )
                , ( "content"
                  , div
                        [ class "dropdown--content-container" ]
                        [ content ]
                  )
                ]
