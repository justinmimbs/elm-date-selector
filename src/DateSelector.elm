module DateSelector exposing (
  Model, init, value,
  Msg(SelectDate), update,
  view
  )

import Date exposing (Date, Month(..), year, month, day)
import Date.Extra as Date exposing (Interval(..))
import Date.Extra.Facts exposing (isLeapYear, daysInMonth, monthFromMonthNumber)
import Html exposing (Html, Attribute, text, div, table, thead, tbody, tr, th, td, ol, li)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import VirtualDom as Dom exposing (property, on)


chunk : Int -> List a -> List (List a)
chunk n list =
  if List.isEmpty list then
    []
  else
    List.take n list :: chunk n (List.drop n list)


isBetween : comparable -> comparable -> comparable -> Bool
isBetween a b x =
  a <= x && x <= b || b <= x && x <= a


monthDates : Int -> Month -> List Date
monthDates y m =
  let
    _ = Debug.log "monthDates" (y, m)
    start = Date.floor Monday <| Date.fromCalendarDate y m 1
  in
    Date.range Day 1 start <| Date.add Day 42 start


-- Model

type Model =
  DateSelector
    { min: Date
    , max: Date
    , selected: Date
    -- cached to avoid repeated consecutive calls to `monthDates`
    , selectedMonthDates: List Date
    }


init : Date -> Date -> Date -> Model
init min max selected =
  let
    min' = min |> Date.floor Day
    max' = max |> Date.floor Day
    selected' = selected |> Date.floor Day |> Date.clamp min' max'
  in
    DateSelector
      { min = min'
      , max = max'
      , selected = selected'
      , selectedMonthDates = monthDates (year selected') (month selected')
      }


value : Model -> Date
value (DateSelector { selected }) =
  selected


-- Update

type Msg
  = SelectYear Int
  | SelectMonth Month
  | SelectDateIndex Int
  -- not used internally, but exposed for external use
  | SelectDate Date


update : Msg -> Model -> Model
update msg (DateSelector a) =
  case msg of
    SelectYear y ->
      let
        m = month a.selected
        d = day a.selected
        date =
          if m == Feb && d == 29 && not (isLeapYear y) then
            Date.fromCalendarDate y Feb 28
          else
            Date.fromCalendarDate y m d
      in
        updateSelected date a

    SelectMonth m ->
      let
        y = year a.selected
        d = day a.selected
        date = Date.fromCalendarDate y m <| min d (daysInMonth y m)
      in
        updateSelected date a

    SelectDateIndex index ->
      let
        date = Maybe.withDefault a.selected (List.head <| List.drop index a.selectedMonthDates)
      in
        updateSelected date a

    SelectDate date ->
      updateSelected (Date.floor Day date) a


updateSelected : Date -> { min: Date, max: Date, selected: Date, selectedMonthDates: List Date } -> Model
updateSelected date a =
  let
    selected = Date.clamp a.min a.max date
    selectedMonthDates =
      if Date.equalBy Month selected a.selected then
        a.selectedMonthDates
      else
        monthDates (year selected) (month selected)
  in
    DateSelector { a | selected = selected, selectedMonthDates = selectedMonthDates }


-- View

view : Model -> Html Msg
view model =
  div
    [ class "date-selector" ]
    [ div
        [ class "column year" ]
        [ viewYearList model ]
    , div
        [ class "column month" ]
        [ viewMonthList model ]
    , div
        [ class "column date" ]
        [ viewDateTable model ]
    ]


viewYearList : Model -> Html Msg
viewYearList (DateSelector a) =
  let
    years = [ year a.min .. year a.max ]
    selectedYear = year a.selected
  in
    ol
      [ on "click" <| Json.Decode.at ["target", "year"] Json.Decode.int ]
      (years |> List.map (\y ->
        li
          [ classList [ ("selected", y == selectedYear) ]
          , property "year" <| Json.Encode.int y
          ]
          [ text (toString y) ]))
      |> Dom.map SelectYear


viewMonthList : Model -> Html Msg
viewMonthList (DateSelector a) =
  let
    first = if year a.selected == year a.min then Date.monthNumber a.min else 1
    last = if year a.selected == year a.max then Date.monthNumber a.max else 12
  in
    ol
      [ on "click" <| Json.Decode.at [ "target", "monthNumber" ] Json.Decode.int ]
      ([ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ] |>
        List.indexedMap (\i name ->
          let
            n = i + 1
          in
            li
              [ classList
                  [ ("selected", n == Date.monthNumber a.selected)
                  , ("disabled", not <| isBetween first last n)
                  ]
              , property "monthNumber" <| Json.Encode.int n
              ]
              [ text name ]))
      |> Dom.map (SelectMonth << monthFromMonthNumber)


viewDateTable : Model -> Html Msg
viewDateTable (DateSelector a) =
  let
    weeks = a.selectedMonthDates |> chunk 7
  in
    table []
      [ thead []
          [ tr []
              ([ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ] |> List.map (\s ->
                th [] [ text s ]))
          ]
      , tbody
          [ on "click" <| Json.Decode.at [ "target", "dateIndex" ] Json.Decode.int ]
          (weeks |> List.indexedMap (\i week ->
            tr []
              (week |> List.indexedMap (\j date ->
                td
                  [ classList
                      [ ("selected", Date.equal date a.selected)
                      , ("dimmed", month date /= month a.selected)
                      , ("disabled", not <| Date.isBetween a.min a.max date)
                      ]
                  , property "dateIndex" <| Json.Encode.int (i * 7 + j)
                  ]
                  [ text (day date |> toString) ]))))
          |> Dom.map SelectDateIndex
      ]
