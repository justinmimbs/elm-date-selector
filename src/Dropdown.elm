module Dropdown exposing (Model, init, Msg(Content), update, view)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


-- Model

type alias Model a =
  { isOpen: Bool
  , content: a
  }


init : a -> Model a
init content =
  Model False content


-- Update

type Msg b
  = Toggle
  | Content b


update : (b -> a -> a) -> (Msg b) -> (Model a) -> (Model a)
update updateContent msg model =
  case msg of
    Toggle ->
      { model | isOpen = not model.isOpen }
    Content b ->
      { model | content = updateContent b model.content }


-- View

view : (Bool -> a -> Html c) -> (a -> Html b) -> (Model a) -> Html (Msg b)
view viewButton viewContent model =
  let
    button =
      div
        [ class "button-container"
        , onClick Toggle
        ]
        [ App.map (\_ -> Toggle) <| viewButton model.isOpen model.content ]
  in
    div
      [ classList
          [ ("dropdown", True)
          , ("open", model.isOpen)
          ]
      ] <|
      if not model.isOpen then
        [ button ]
      else
        [ div
            [ class "page-cover"
            , onClick Toggle
            ]
            []
        , button
        , div
            [ class "content-container" ]
            [ App.map Content (viewContent model.content) ]
        ]
