import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateSelectorDropdown as DateSelector
import Html exposing (Html, div, button, text, ol, li, pre, h2)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program Never
main =
  App.beginnerProgram
  { model =
      { dateInput1 = DateSelector.init
          (Date.fromCalendarDate 2011 Mar 15)
          (Date.fromCalendarDate 2017 Sep 15)
          (Date.fromCalendarDate 2016 Jun 8)
      , dateInput2 = DateSelector.init
          (Date.fromCalendarDate 2011 Mar 15)
          (Date.fromCalendarDate 2017 Sep 15)
          (Date.fromCalendarDate 2017 Jun 7)
      }
  , view = view
  , update = update
  }


-- Model

type alias Model =
  { dateInput1 : DateSelector.Model
  , dateInput2 : DateSelector.Model
  }


-- Update

type Field
  = Date1
  | Date2


type FieldAction
  = Input DateSelector.Msg
  | Reset


type Msg
  = Form Field FieldAction


update : Msg -> Model -> Model
update (Form field action) model =
  let
    msg =
      case action of
        Input m -> m
        Reset -> DateSelector.SelectDate (Date.fromCalendarDate 2016 Jun 8)
  in
    case field of
      Date1 -> { model | dateInput1 = DateSelector.update msg model.dateInput1 }
      Date2   -> { model | dateInput2 = DateSelector.update msg model.dateInput2 }


-- View

dateSelectorButton : Bool -> Date -> Html a
dateSelectorButton isOpen date =
  div
    [ style
        [ ("display", "inline-block")
        , ("padding", "5px 10px")
        , ("font-family", "Menlo, Consolas, monospace")
        , ("border", "1px solid " ++ if isOpen then "#bbbbbb" else "#dddddd")
        , ("background", "#ffffff")
        ]
    ]
    [ text <| Date.toFormattedString "yyyy MMM dd" date ]


view : Model -> Html Msg
view model =
  div
    [ style [ ("margin", "20px") ] ]
    [ div
        []
        [ h2 [] [ text <| "Default button view" ]
        , App.map ((Form Date1) << Input) <| DateSelector.view model.dateInput1
        , button
            [ onClick (Form Date1 Reset), style [ ("margin-left", "10px") ] ]
            [ text "Reset" ]
        ]
    , div
        []
        [ h2 [] [ text <| "Custom button view" ]
        , App.map ((Form Date2) << Input) <| DateSelector.viewWithButton dateSelectorButton model.dateInput2
        , button
            [ onClick (Form Date2 Reset), style [ ("margin-left", "10px") ] ]
            [ text "Reset" ]
        ]
    ]
