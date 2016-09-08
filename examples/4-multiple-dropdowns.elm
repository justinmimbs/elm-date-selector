import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(Year, Month, Day))
import DateSelectorDropdown
import Html exposing (Html, div, text, h1, br, label)
import Html.App as App
import Html.Attributes exposing (class, style)
import String


main : Program Never
main =
  App.beginnerProgram
  { model = init <| Date.fromCalendarDate 2016 Sep 15
  , view = view
  , update = update
  }


-- model

type DateField
  = From
  | To
  | Birthdate


type alias Model =
  { today : Date
  , from : Date
  , to : Date
  , birthdate : Maybe Date
  , openDateField : Maybe DateField
  }


init : Date -> Model
init today =
  Model
    today
    (Date.floor Month today)
    (Date.ceiling Month today |> Date.add Day -1)
    Nothing
    Nothing


-- update

type Msg
  = Select DateField Date
  | Toggle DateField


update : Msg -> Model -> Model
update msg model =
  case msg of
    Select dateField date ->
      case dateField of
        From      -> { model | from = date }
        To        -> { model | to = date }
        Birthdate -> { model | birthdate = Just date }

    Toggle dateField ->
      { model
      | openDateField =
          case model.openDateField of
            Just _  -> Nothing
            Nothing -> Just dateField
      }


-- view

view : Model -> Html Msg
view { today, from, to, birthdate, openDateField } =
  div []
    [ Html.node "style" []
        [ text <| String.join " "
            [ "@import url(./examples.css);"
            , "@import url(./date-selector-dropdown.css);"
            ]
        ]
    , div
        [ class "column" ]
        [ label [] [ text "From" ]
        , viewDateSelector From openDateField
            (Date.add Year -10 today)
            to
            (Just from)
        ]
    , div
        [ class "column" ]
        [ label [] [ text "To" ]
        , viewDateSelector To openDateField
            from
            (Date.add Year 1 today)
            (Just to)
        ]
    , div [ style [ ("clear", "both") ] ] []

    , label [] [ text "Birthdate" ]
    , viewDateSelector Birthdate openDateField
        (Date.add Year -110 today)
        today
        birthdate
    ]


viewDateSelector : DateField -> (Maybe DateField) -> Date -> Date -> Maybe Date -> Html Msg
viewDateSelector dateField openDateField =
  DateSelectorDropdown.view
    (Toggle dateField)
    (Select dateField)
    (openDateField |> Maybe.map ((==) dateField) |> Maybe.withDefault False)
