module Dropdown exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
        div
          [ class "dropdown" ]
          [ buttonContainer ]

      Just content ->
        div
          [ class "dropdown-open" ]
          [ div
              [ class "dropdown--page-cover"
              , onClick close
              ]
              []
          , buttonContainer
          , div
              [ class "dropdown--content-container" ]
              [ content ]
          ]
