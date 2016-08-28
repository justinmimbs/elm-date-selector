import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateSelectorDropdown
import Html exposing (Html, div, button, text, h1, span, input)
import Html.App as App
import Html.Attributes exposing (class, classList, type', readonly, value)
import Html.Events exposing (onClick)
import String


main : Program Never
main =
  App.beginnerProgram
  { model =
      Model
        (Date.fromCalendarDate 2011 Mar 15)
        (Date.fromCalendarDate 2017 Sep 15)
        (Date.fromCalendarDate 2016 Jun 8)
        False
  , view = view
  , update = update
  }


-- Model

type alias Model =
  { min : Date
  , max : Date
  , selected : Date
  , isOpen : Bool
  }


-- Update

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


-- View

view : Model -> Html Msg
view { min, max, selected, isOpen } =
  div []
    [ Html.node "style" [] 
        [ text <| String.join " "
            [ "@import url(./style.css);"
            , "@import url(./date-selector-dropdown.css);"
            ]
        ]
    , h1 [] [ text <| Date.toFormattedString "EEE MMM d, yyyy" selected ]
    , DateSelectorDropdown.view Toggle Select min max selected isOpen
    ]
