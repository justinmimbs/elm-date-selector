module DateSelector exposing (view)

{-| Create a user interface for selecting dates.

@docs view

-}

import Date exposing (Date, Month(..), day, month, year)
import Date.Extra as Date exposing (Interval(..), numberToMonth)
import Html exposing (Html, div, li, ol, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, property)
import Html.Events exposing (on)
import Json.Decode
import Json.Encode


groupsOf : Int -> List a -> List (List a)
groupsOf n list =
    if List.isEmpty list then
        []
    else
        List.take n list :: groupsOf n (List.drop n list)


isBetween : comparable -> comparable -> comparable -> Bool
isBetween a b x =
    a <= x && x <= b


monthDates : Int -> Month -> List Date
monthDates y m =
    let
        start =
            Date.floor Monday <| Date.fromCalendarDate y m 1
    in
    Date.range Day 1 start <| Date.add Day 42 start


dateWithYear : Date -> Int -> Date
dateWithYear date y =
    Date.fromCalendarDate y (month date) (day date)


dateWithMonth : Date -> Month -> Date
dateWithMonth date m =
    Date.fromCalendarDate (year date) m (day date)



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
        Normal ->
            ""

        Dimmed ->
            "date-selector--dimmed"

        Disabled ->
            "date-selector--disabled"

        Selected ->
            "date-selector--selected"


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
            [ ( "date-selector", True )
            , ( "date-selector--scrollable-year", year maximum - year minimum >= 12 )
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
            [ case maybeSelected of
                Just selected ->
                    viewDateTable minimum maximum selected

                Nothing ->
                    viewDateTableDisabled minimum
            ]
        ]
        |> Html.map (Date.clamp minimum maximum)


viewYearList : Date -> Date -> Maybe Date -> Html Date
viewYearList minimum maximum maybeSelected =
    let
        isInvertedMinMax =
            Date.compare minimum maximum == GT

        years =
            if isInvertedMinMax then
                [ maybeSelected |> Maybe.withDefault minimum |> year ]
            else
                List.range (year minimum) (year maximum)

        isSelectedYear : Int -> Bool
        isSelectedYear =
            maybeSelected
                |> Maybe.map (\selected -> (==) (year selected))
                |> Maybe.withDefault (\_ -> False)
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (dateWithYear (maybeSelected |> Maybe.withDefault (Date.fromCalendarDate (year minimum) Jan 1)))
                (Json.Decode.at [ "target", "data-year" ] Json.Decode.int)
        ]
        (years
            |> List.map
                (\y ->
                    let
                        state =
                            if isSelectedYear y then
                                Selected
                            else if isInvertedMinMax then
                                Disabled
                            else
                                Normal
                    in
                    li
                        [ class <| classNameFromState state
                        , property "data-year" <|
                            if isSelectable state then
                                Json.Encode.int y
                            else
                                Json.Encode.null
                        ]
                        [ text (toString y) ]
                )
        )


monthNames : List String
monthNames =
    [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]


viewMonthList : Date -> Date -> Date -> Html Date
viewMonthList minimum maximum selected =
    let
        isInvertedMinMax =
            Date.compare minimum maximum == GT

        first =
            if year selected == year minimum then
                Date.monthNumber minimum
            else
                1

        last =
            if year selected == year maximum then
                Date.monthNumber maximum
            else
                12
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (numberToMonth >> dateWithMonth selected)
                (Json.Decode.at [ "target", "data-month" ] Json.Decode.int)
        ]
        (monthNames
            |> List.indexedMap
                (\i name ->
                    let
                        n =
                            i + 1

                        state =
                            if n == Date.monthNumber selected then
                                Selected
                            else if not (isBetween first last n) || isInvertedMinMax then
                                Disabled
                            else
                                Normal
                    in
                    li
                        [ class <| classNameFromState state
                        , property "data-month" <|
                            if isSelectable state then
                                Json.Encode.int n
                            else
                                Json.Encode.null
                        ]
                        [ text name ]
                )
        )


viewMonthListDisabled : Html a
viewMonthListDisabled =
    ol []
        (monthNames
            |> List.map
                (\name ->
                    li
                        [ class <| classNameFromState Disabled ]
                        [ text name ]
                )
        )


dayOfWeekNames : List String
dayOfWeekNames =
    [ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]


viewDayOfWeekHeader : Html a
viewDayOfWeekHeader =
    thead []
        [ tr []
            (dayOfWeekNames
                |> List.map
                    (\name ->
                        th [] [ text name ]
                    )
            )
        ]


viewDateTable : Date -> Date -> Date -> Html Date
viewDateTable minimum maximum selected =
    let
        isInvertedMinMax =
            Date.compare minimum maximum == GT

        weeks =
            monthDates (year selected) (month selected) |> groupsOf 7
    in
    table []
        [ viewDayOfWeekHeader
        , tbody
            [ on "click" <|
                Json.Decode.map
                    Date.fromTime
                    (Json.Decode.at [ "target", "data-time" ] Json.Decode.float)
            ]
            (weeks
                |> List.map
                    (\week ->
                        tr []
                            (week
                                |> List.map
                                    (\date ->
                                        let
                                            state =
                                                if Date.equalBy Day date selected then
                                                    Selected
                                                else if not (Date.isBetween minimum maximum date) || isInvertedMinMax then
                                                    Disabled
                                                else if month date /= month selected then
                                                    Dimmed
                                                else
                                                    Normal
                                        in
                                        td
                                            [ class <| classNameFromState state
                                            , property "data-time" <|
                                                if isSelectable state then
                                                    Json.Encode.float (Date.toTime date)
                                                else
                                                    Json.Encode.null
                                            ]
                                            [ text (day date |> toString) ]
                                    )
                            )
                    )
            )
        ]


viewDateTableDisabled : Date -> Html a
viewDateTableDisabled date =
    let
        weeks =
            monthDates (year date) (month date) |> groupsOf 7

        disabled =
            classNameFromState Disabled
    in
    table []
        [ viewDayOfWeekHeader
        , tbody []
            (weeks
                |> List.map
                    (\week ->
                        tr []
                            (week
                                |> List.map
                                    (\date ->
                                        td
                                            [ class disabled ]
                                            [ text (day date |> toString) ]
                                    )
                            )
                    )
            )
        ]
