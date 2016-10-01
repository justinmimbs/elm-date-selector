module DateSelector exposing (view)

{-| Create a user interface for selecting dates.

@docs view
-}

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


{-| Create a date selector by providing the minimum and maximum selectable
dates, and a selected date if there is one.

    DateSelector.view
      minimum
      maximum
      selected

The resulting `Html` produces `Date` messages when the user selects a date. The
`Date` values produced will always be within the bounds provided.
-}
view : Date -> Date -> Maybe Date -> Html Date
view minimum maximum maybeSelected =
  div
    [ classList
        [ ("date-selector", True)
        , ("date-selector--scrollable-year", year maximum - year minimum >= 12)
        ]
    ]
    [ div []
        [ viewYearList minimum maximum maybeSelected ]
    , div []
        [ maybeSelected
          |> Maybe.map (viewMonthList minimum maximum)
          |> Maybe.withDefault viewMonthListDisabled
        ]
    , div []
        [ maybeSelected
          |> Maybe.map (viewDateTable minimum maximum)
          |> Maybe.withDefault (viewDateTableDisabled minimum)
        ]
    ]
    |> App.map (Date.clamp minimum maximum)


viewYearList : Date -> Date -> Maybe Date -> Html Date
viewYearList minimum maximum maybeSelected =
  let
    years = [ year minimum .. year maximum ]
    isSelectedYear =
      maybeSelected
        |> Maybe.map (\selected -> (==) (year selected))
        |> Maybe.withDefault (\_ -> False)
  in
    ol
      [ on "click" <|
        Json.Decode.map
          (dateWithYear (maybeSelected |> Maybe.withDefault (Date.fromCalendarDate (year minimum) Jan 1)))
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
viewMonthList minimum maximum selected =
  let
    first = if year selected == year minimum then Date.monthNumber minimum else 1
    last = if year selected == year maximum then Date.monthNumber maximum else 12
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
viewDateTable minimum maximum selected =
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
                    if Date.equalBy Day date selected then
                      Selected
                    else if not (Date.isBetween minimum maximum date) then
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
