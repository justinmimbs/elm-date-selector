module DateSelector exposing (view)

{-| Create a user interface for selecting dates.

@docs view

-}

import Date.RataDie as Date exposing (Date, Interval(Day, Monday), Month(..), Unit(Days))
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
    a <= x && x <= b || b <= x && x <= a


monthDates : Int -> Month -> List Date
monthDates y m =
    let
        start =
            Date.firstOfMonth y m |> Date.floor Monday
    in
    Date.range Day 1 start (Date.add Days 42 start)


dateWithYear : Date -> Int -> Date
dateWithYear date year =
    let
        { month, day } =
            Date.toCalendarDate date
    in
    Date.fromCalendarDate year month day


dateWithMonth : Date -> Month -> Date
dateWithMonth date month =
    let
        { year, day } =
            Date.toCalendarDate date
    in
    Date.fromCalendarDate year month day



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
            , ( "date-selector--scrollable-year", Date.year maximum - Date.year minimum >= 12 )
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
        |> Html.map (clamp minimum maximum)


viewYearList : Date -> Date -> Maybe Date -> Html Date
viewYearList minimum maximum maybeSelected =
    let
        isInvertedMinMax =
            minimum > maximum

        years =
            if isInvertedMinMax then
                [ maybeSelected |> Maybe.withDefault minimum |> Date.year ]
            else
                List.range (Date.year minimum) (Date.year maximum)

        isSelectedYear : Int -> Bool
        isSelectedYear =
            maybeSelected
                |> Maybe.map (\selected -> (==) (Date.year selected))
                |> Maybe.withDefault (\_ -> False)
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (dateWithYear (maybeSelected |> Maybe.withDefault (Date.firstOfYear (Date.year minimum))))
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
            minimum > maximum

        first =
            if Date.year selected == Date.year minimum then
                Date.monthNumber minimum
            else
                1

        last =
            if Date.year selected == Date.year maximum then
                Date.monthNumber maximum
            else
                12
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (Date.numberToMonth >> dateWithMonth selected)
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


weekdayNames : List String
weekdayNames =
    [ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]


viewWeekdayHeader : Html a
viewWeekdayHeader =
    thead []
        [ tr []
            (weekdayNames
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
            minimum > maximum

        weeks =
            monthDates (Date.year selected) (Date.month selected) |> groupsOf 7
    in
    table []
        [ viewWeekdayHeader
        , tbody
            [ on "click" <|
                Json.Decode.map
                    identity
                    (Json.Decode.at [ "target", "data-date" ] Json.Decode.int)
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
                                                if date == selected then
                                                    Selected
                                                else if not (date |> isBetween minimum maximum) || isInvertedMinMax then
                                                    Disabled
                                                else if Date.monthNumber date /= Date.monthNumber selected then
                                                    Dimmed
                                                else
                                                    Normal
                                        in
                                        td
                                            [ class <| classNameFromState state
                                            , property "data-date" <|
                                                if isSelectable state then
                                                    Json.Encode.int date
                                                else
                                                    Json.Encode.null
                                            ]
                                            [ text (Date.day date |> toString) ]
                                    )
                            )
                    )
            )
        ]


viewDateTableDisabled : Date -> Html a
viewDateTableDisabled date =
    let
        weeks =
            monthDates (Date.year date) (Date.month date) |> groupsOf 7

        disabled =
            classNameFromState Disabled
    in
    table []
        [ viewWeekdayHeader
        , tbody [] <|
            List.map
                (\weekdates ->
                    tr [] <|
                        List.map
                            (\date ->
                                td
                                    [ class disabled ]
                                    [ text (Date.day date |> toString) ]
                            )
                            weekdates
                )
                weeks
        ]
