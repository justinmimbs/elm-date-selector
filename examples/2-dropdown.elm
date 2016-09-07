import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateSelectorDropdown
import Html exposing (Html, div, text, h1)
import Html.App as App
import String


main : Program Never
main =
  App.beginnerProgram
  { model =
      Model
        (Date.fromCalendarDate 2011 Mar 15)
        (Date.fromCalendarDate 2017 Sep 15)
        (Date.fromCalendarDate 2016 Sep 15)
        False
  , view = view
  , update = update
  }


-- model

type alias Model =
  { min : Date
  , max : Date
  , selected : Date
  , isOpen : Bool
  }


-- update

type Msg
  = Select Date
  | Toggle


update : Msg -> Model -> Model
update msg model =
  case msg of
    Select date ->
      { model | selected = date }
    Toggle ->
      { model | isOpen = not model.isOpen }


-- view

view : Model -> Html Msg
view { min, max, selected, isOpen } =
  div []
    [ Html.node "style" []
        [ text <| String.join " "
            [ "@import url(./examples.css);"
            , "@import url(./date-selector-dropdown.css);"
            ]
        ]
    , h1 [] [ text <| Date.toFormattedString "EEE MMM d, yyyy" selected ]
    , DateSelectorDropdown.view
        Toggle
        Select
        isOpen
        min
        max
        selected
    ]
