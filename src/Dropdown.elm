module Dropdown exposing (view)

import Html exposing (Html, div)
import Html.App as App
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


view : msg -> Html a -> Maybe (Html msg) -> Html msg
view toggle button maybeContent =
  let
    buttonContainer =
      div
        [ class "button-container"
        , onClick toggle
        ]
        [ button |> App.map (\_ -> toggle) ]
  in
    case maybeContent of
      Nothing ->
        div
          [ class "dropdown" ]
          [ buttonContainer ]

      Just content ->
        div
          [ class "dropdown open" ]
          [ div
              [ class "page-cover"
              , onClick toggle
              ]
              []
          , buttonContainer
          , div
              [ class "content-container" ]
              [ content ]
          ]
