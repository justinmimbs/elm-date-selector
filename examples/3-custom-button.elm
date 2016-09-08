import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateSelectorDropdown
import Html exposing (Html, div, h1, span, text)
import Html.App as App
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import String


main : Program Never
main =
  App.beginnerProgram
  { model =
      Model
        (Date.fromCalendarDate 2011 Mar 15)
        (Date.fromCalendarDate 2017 Sep 15)
        (Just <| Date.fromCalendarDate 2016 Sep 15)
        False
  , view = view
  , update = update
  }


-- model

type alias Model =
  { min : Date
  , max : Date
  , maybeSelected : Maybe Date
  , isOpen : Bool
  }


-- update

type Msg
  = Select (Maybe Date)
  | Toggle


update : Msg -> Model -> Model
update msg model =
  case msg of
    Select maybeDate ->
      { model | maybeSelected = maybeDate }
    Toggle ->
      { model | isOpen = not model.isOpen }


-- view

view : Model -> Html Msg
view { min, max, maybeSelected, isOpen } =
  div []
    [ Html.node "style" []
        [ text <| String.join " "
            [ "@import url(./examples.css);"
            , "@import url(./date-selector-dropdown.css);"
            , "@import url(./custom-button.css);"
            ]
        ]
    , h1 [] [ text (maybeSelected |> Maybe.map (Date.toFormattedString "'Just' (EEE MMM d, yyyy)") |> Maybe.withDefault "Nothing") ]
    , DateSelectorDropdown.viewWithButton
        viewCustomButton
        Toggle
        (Select << Just)
        isOpen
        min
        max
        maybeSelected
    ]


viewCustomButton : Bool -> Maybe Date -> Html Msg
viewCustomButton isOpen maybeDate =
  div
    [ classList
        [ ("date-selector-dropdown-button", True)
        , ("date-selector-dropdown-button--open", isOpen)
        ]

    ]
    [ div
        [ class "date-selector-dropdown-button--date" ]
        [ text (maybeDate |> Maybe.map (Date.toFormattedString "yyyy MMM d") |> Maybe.withDefault "") ]

    , case maybeDate of
        Just _ ->
          div
            [ class "date-selector-dropdown-button--clear"
            , onClick (Select Nothing)
            ]
            [ text "\x2716" ]

        Nothing ->
          div
            [ class "date-selector-dropdown-button--clear" ]
            []

    , div
        [ class "date-selector-dropdown-button--toggle"
        , onClick Toggle
        ]
        [ span [] [ text <| if isOpen then "\x25B2" else "\x25BC" ] ]
    ]
