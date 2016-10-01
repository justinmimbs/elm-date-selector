import Date exposing (Date, Month(..))
import Date.Extra as Date exposing (Interval(Year, Month, Day))
import DateSelectorDropdown
import Html exposing (Html, div, text, h1, label)
import Html.App as App
import Html.Attributes exposing (class)
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
  | OpenDropdown DateField
  | CloseDropdown


update : Msg -> Model -> Model
update msg model =
  case msg of
    Select dateField date ->
      case dateField of
        From      -> { model | from = date }
        To        -> { model | to = date }
        Birthdate -> { model | birthdate = Just date }

    OpenDropdown dateField ->
      { model | openDateField = Just dateField }

    CloseDropdown ->
      { model | openDateField = Nothing }


-- view

view : Model -> Html Msg
view { today, from, to, birthdate, openDateField } =
  div []
    [ Html.node "style" []
        [ text <| String.join " "
            [ "@import url(./examples.css);"
            , "@import url(./date-selector.css);"
            ]
        ]
    , div
        [ class "columns" ]
        [ div []
            [ label [] [ text "From" ]
            , viewDateSelector From openDateField
                (Date.add Year -10 today)
                to
                (Just from)
            ]
        , div []
            [ label [] [ text "To" ]
            , viewDateSelector To openDateField
                from
                (Date.add Year 1 today)
                (Just to)
            ]
        ]
    , label [] [ text "Birthdate" ]
    , viewDateSelector Birthdate openDateField
        (Date.add Year -110 today)
        today
        birthdate
    ]


viewDateSelector : DateField -> (Maybe DateField) -> Date -> Date -> Maybe Date -> Html Msg
viewDateSelector dateField openDateField =
  let
    isOpen = openDateField |> Maybe.map ((==) dateField) |> Maybe.withDefault False
  in
    DateSelectorDropdown.view
      (if isOpen then CloseDropdown else OpenDropdown dateField)
      (Select dateField)
      isOpen
