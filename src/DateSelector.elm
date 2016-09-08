module DateSelector exposing (view)

import Date exposing (Date, Month(..), year, month, day)
import Date.Extra as Date exposing (Interval(..))
import Date.Extra.Facts exposing (isLeapYear, daysInMonth, monthFromMonthNumber)
import Html exposing (Html, text, div, table, thead, tbody, tr, th, td, ol, li)
import Html.App as App
import Html.Attributes exposing (class, classList, property)
import Html.Events exposing (on)
import Json.Decode
import Json.Encode


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
    start = Date.floor Monday <| Date.fromCalendarDate y m 1
  in
    Date.range Day 1 start <| Date.add Day 42 start


dateWithYear : Date -> Int -> Date
dateWithYear date y =
  let
    m = month date
    d = day date
  in
    if m == Feb && d == 29 && not (isLeapYear y) then
      Date.fromCalendarDate y Feb 28
    else
      Date.fromCalendarDate y m d


dateWithMonth : Date -> Month -> Date
dateWithMonth date m =
  let
    y = year date
    d = day date
  in
    Date.fromCalendarDate y m <| Basics.min d (daysInMonth y m)


-- view

type State
  = Normal
  | Dimmed
  | Disabled
  | Selected


isSelectable : State -> Bool
isSelectable state =
  state == Normal || state == Dimmed


classNameFromState : State -> String
classNameFromState state =
  case state of
    Normal   -> ""
    Dimmed   -> "date-selector--dimmed"
    Disabled -> "date-selector--disabled"
    Selected -> "date-selector--selected"


view : Date -> Date -> Maybe Date -> Html Date
view min max maybeSelected =
  div
    [ classList
        [ ("date-selector", True)
        , ("date-selector--scrollable-year", year max - year min >= 12)
        ]
    ]
    [ div []
        [ viewYearList min max maybeSelected ]
    , div []
        [ maybeSelected
          |> Maybe.map (viewMonthList min max)
          |> Maybe.withDefault viewMonthListDisabled
        ]
    , div []
        [ maybeSelected
          |> Maybe.map (viewDateTable min max)
          |> Maybe.withDefault (viewDateTableDisabled min)
        ]
    ]
    |> App.map (Date.clamp min max)


viewYearList : Date -> Date -> Maybe Date -> Html Date
viewYearList min max maybeSelected =
  let
    years = [ year min .. year max ]
    isSelectedYear =
      maybeSelected
        |> Maybe.map (\selected -> (==) (year selected))
        |> Maybe.withDefault (\_ -> False)
  in
    ol
      [ on "click" <|
        Json.Decode.map
          (dateWithYear (maybeSelected |> Maybe.withDefault (Date.fromCalendarDate (year min) Jan 1)))
          (Json.Decode.at ["target", "year"] Json.Decode.int)
      ]
      (years |> List.map (\y ->
        let
          state =
            if isSelectedYear y then
              Selected
            else
              Normal
        in
          li
            ([ class <| classNameFromState state ]
              ++
              if isSelectable state then
                [ property "year" <| Json.Encode.int y ]
              else
                [])
            [ text (toString y) ]))


monthNames : List String
monthNames =
  [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]


viewMonthList : Date -> Date -> Date -> Html Date
viewMonthList min max selected =
  let
    first = if year selected == year min then Date.monthNumber min else 1
    last = if year selected == year max then Date.monthNumber max else 12
  in
    ol
      [ on "click" <|
        Json.Decode.map
          (dateWithMonth selected << monthFromMonthNumber)
          (Json.Decode.at [ "target", "monthNumber" ] Json.Decode.int)
      ]
      (monthNames |> List.indexedMap (\i name ->
        let
          n = i + 1
          state =
            if n == Date.monthNumber selected then
              Selected
            else if not (isBetween first last n) then
              Disabled
            else
              Normal
        in
          li
            ([ class <| classNameFromState state ]
              ++
              if isSelectable state then
                [ property "monthNumber" <| Json.Encode.int n ]
              else
                [])
            [ text name ]))


viewMonthListDisabled : Html a
viewMonthListDisabled =
  ol []
    (monthNames |> List.map (\name ->
      li
        [ class <| classNameFromState Disabled ]
        [ text name ]))


dayOfWeekNames : List String
dayOfWeekNames =
  [ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]


viewDayOfWeekHeader : Html a
viewDayOfWeekHeader =
  thead []
    [ tr []
        (dayOfWeekNames |> List.map (\name ->
          th [] [ text name ]))
    ]


viewDateTable : Date -> Date -> Date -> Html Date
viewDateTable min max selected =
  let
    weeks = monthDates (year selected) (month selected) |> chunk 7
  in
    table []
      [ viewDayOfWeekHeader
      , tbody
          [ on "click" <|
              Json.Decode.map
                Date.fromTime
                (Json.Decode.at [ "target", "time" ] Json.Decode.float)
          ]
          (weeks |> List.map (\week ->
            tr []
              (week |> List.map (\date ->
                let
                  state =
                    if Date.equal date selected then
                      Selected
                    else if not (Date.isBetween min max date) then
                      Disabled
                    else if month date /= month selected then
                      Dimmed
                    else
                      Normal
                in
                  td
                    ([ class <| classNameFromState state ]
                      ++
                      if isSelectable state then
                        [ property "time" <| Json.Encode.float (Date.toTime date) ]
                      else
                        [])
                    [ text (day date |> toString) ]))))
      ]


viewDateTableDisabled : Date -> Html a
viewDateTableDisabled date =
  let
    weeks = monthDates (year date) (month date) |> chunk 7
    disabled = classNameFromState Disabled
  in
    table []
      [ viewDayOfWeekHeader
      , tbody []
          (weeks |> List.map (\week ->
            tr []
              (week |> List.map (\date ->
                td
                  [ class disabled ]
                  [ text (day date |> toString) ]))))
      ]
