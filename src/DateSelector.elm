module DateSelector exposing (view)

{-| Create a user interface for selecting dates.

@docs view

-}

import Date exposing (Date, Interval(..), Month, Unit(..))
import Html exposing (Html)
import Html.Attributes
import Html.Events
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


clampDate : Date -> Date -> Date -> Date
clampDate minDate maxDate date =
    Date.fromRataDie <|
        clamp
            (Date.toRataDie minDate)
            (Date.toRataDie maxDate)
            (Date.toRataDie date)


compareDate : Date -> Date -> Order
compareDate left right =
    compare (Date.toRataDie left) (Date.toRataDie right)


isBetweenDate : Date -> Date -> Date -> Bool
isBetweenDate minDate maxDate date =
    isBetween
        (Date.toRataDie minDate)
        (Date.toRataDie maxDate)
        (Date.toRataDie date)


monthDates : Int -> Month -> List Date
monthDates y m =
    let
        start =
            Date.fromCalendarDate y m 1 |> Date.floor Monday
    in
    Date.range Day 1 start (Date.add Days 42 start)


dateWithYear : Date -> Int -> Date
dateWithYear date year =
    Date.fromCalendarDate
        year
        (Date.month date)
        (Date.day date)


dateWithMonth : Date -> Month -> Date
dateWithMonth date month =
    Date.fromCalendarDate
        (Date.year date)
        month
        (Date.day date)



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
view minDate maxDate maybeSelected =
    Html.div
        [ Html.Attributes.classList
            [ ( "date-selector", True )
            , ( "date-selector--scrollable-year", Date.year maxDate - Date.year minDate >= 12 )
            ]
        ]
        [ Html.div []
            [ viewYearList minDate maxDate maybeSelected ]
        , Html.div []
            [ maybeSelected
                |> Maybe.map (viewMonthList minDate maxDate)
                |> Maybe.withDefault viewMonthListDisabled
            ]
        , Html.div []
            [ case maybeSelected of
                Just selected ->
                    viewDateTable minDate maxDate selected

                Nothing ->
                    viewDateTableDisabled minDate
            ]
        ]
        |> Html.map (clampDate minDate maxDate)


viewYearList : Date -> Date -> Maybe Date -> Html Date
viewYearList minDate maxDate maybeSelected =
    let
        isInvertedMinMax =
            compareDate minDate maxDate == GT

        years =
            if isInvertedMinMax then
                [ Date.year (maybeSelected |> Maybe.withDefault minDate) ]

            else
                List.range (Date.year minDate) (Date.year maxDate)

        isSelectedYear : Int -> Bool
        isSelectedYear =
            maybeSelected
                |> Maybe.map (\selected -> (==) (Date.year selected))
                |> Maybe.withDefault (always False)
    in
    Html.ol
        [ Html.Events.on "click" <|
            Json.Decode.map
                (dateWithYear (maybeSelected |> Maybe.withDefault (Date.fromOrdinalDate (Date.year minDate) 1)))
                (Json.Decode.at [ "target", "data-year" ] Json.Decode.int)
        ]
        (years
            |> List.map
                (\year ->
                    let
                        state =
                            if isSelectedYear year then
                                Selected

                            else if isInvertedMinMax then
                                Disabled

                            else
                                Normal
                    in
                    Html.li
                        [ Html.Attributes.class <| classNameFromState state
                        , Html.Attributes.property "data-year" <|
                            if isSelectable state then
                                Json.Encode.int year

                            else
                                Json.Encode.null
                        ]
                        [ Html.text (String.fromInt year) ]
                )
        )


monthNames : List String
monthNames =
    [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]


viewMonthList : Date -> Date -> Date -> Html Date
viewMonthList minDate maxDate selected =
    let
        isInvertedMinMax =
            compareDate minDate maxDate == GT

        first =
            if Date.year selected == Date.year minDate then
                Date.monthNumber minDate

            else
                1

        last =
            if Date.year selected == Date.year maxDate then
                Date.monthNumber maxDate

            else
                12
    in
    Html.ol
        [ Html.Events.on "click" <|
            Json.Decode.map
                (Date.numberToMonth >> dateWithMonth selected)
                (Json.Decode.at [ "target", "data-month" ] Json.Decode.int)
        ]
        (monthNames
            |> List.indexedMap
                (\i monthName ->
                    let
                        monthNumber =
                            i + 1

                        state =
                            if monthNumber == Date.monthNumber selected then
                                Selected

                            else if not (isBetween first last monthNumber) || isInvertedMinMax then
                                Disabled

                            else
                                Normal
                    in
                    Html.li
                        [ Html.Attributes.class <| classNameFromState state
                        , Html.Attributes.property "data-month" <|
                            if isSelectable state then
                                Json.Encode.int monthNumber

                            else
                                Json.Encode.null
                        ]
                        [ Html.text monthName ]
                )
        )


viewMonthListDisabled : Html a
viewMonthListDisabled =
    Html.ol []
        (monthNames
            |> List.map
                (\monthName ->
                    Html.li
                        [ Html.Attributes.class <| classNameFromState Disabled ]
                        [ Html.text monthName ]
                )
        )


weekdayNames : List String
weekdayNames =
    [ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]


weekdayHeader : Html a
weekdayHeader =
    Html.thead []
        [ Html.tr []
            (weekdayNames
                |> List.map
                    (\weekdayName ->
                        Html.th [] [ Html.text weekdayName ]
                    )
            )
        ]


viewDateTable : Date -> Date -> Date -> Html Date
viewDateTable minDate maxDate selected =
    let
        isInvertedMinMax =
            compareDate minDate maxDate == GT

        weeks =
            monthDates (Date.year selected) (Date.month selected) |> groupsOf 7
    in
    Html.table []
        [ weekdayHeader
        , Html.tbody
            [ Html.Events.on "click" <|
                Json.Decode.map
                    Date.fromRataDie
                    (Json.Decode.at [ "target", "data-date" ] Json.Decode.int)
            ]
            (weeks
                |> List.map
                    (\dates ->
                        Html.tr []
                            (dates
                                |> List.map
                                    (\date ->
                                        let
                                            state =
                                                if date == selected then
                                                    Selected

                                                else if not (date |> isBetweenDate minDate maxDate) || isInvertedMinMax then
                                                    Disabled

                                                else if Date.month date /= Date.month selected then
                                                    Dimmed

                                                else
                                                    Normal
                                        in
                                        Html.td
                                            [ Html.Attributes.class <| classNameFromState state
                                            , Html.Attributes.property "data-date" <|
                                                if isSelectable state then
                                                    Json.Encode.int (Date.toRataDie date)

                                                else
                                                    Json.Encode.null
                                            ]
                                            [ Html.text <| String.fromInt (Date.day date) ]
                                    )
                            )
                    )
            )
        ]


viewDateTableDisabled : Date -> Html a
viewDateTableDisabled selected =
    let
        weeks =
            monthDates (Date.year selected) (Date.month selected) |> groupsOf 7

        disabled =
            classNameFromState Disabled
    in
    Html.table []
        [ weekdayHeader
        , Html.tbody []
            (weeks
                |> List.map
                    (\dates ->
                        Html.tr []
                            (dates
                                |> List.map
                                    (\date ->
                                        Html.td
                                            [ Html.Attributes.class disabled ]
                                            [ Html.text <| String.fromInt (Date.day date) ]
                                    )
                            )
                    )
            )
        ]
