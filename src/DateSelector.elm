module DateSelector exposing (view)

{-| Create a user interface for selecting dates.

@docs view

-}

import Date.Basic as Date exposing (Date)
import Date.RataDie as RataDie exposing (Interval(Day, Monday), Month(..), RataDie, Unit(Days))
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


monthDates : Int -> Month -> List RataDie
monthDates y m =
    let
        start =
            RataDie.firstOfMonth y m |> RataDie.floor Monday
    in
    RataDie.range Day 1 start (RataDie.add Days 42 start)


dateWithYear : RataDie -> Int -> RataDie
dateWithYear date year =
    let
        { month, day } =
            RataDie.toCalendarDate date
    in
    RataDie.fromCalendarDate year month day


dateWithMonth : RataDie -> Month -> RataDie
dateWithMonth date month =
    let
        { year, day } =
            RataDie.toCalendarDate date
    in
    RataDie.fromCalendarDate year month day



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
view minDate maxDate maybeSelectedDate =
    let
        minimum =
            minDate |> Date.toRataDie

        maximum =
            maxDate |> Date.toRataDie

        maybeSelected =
            maybeSelectedDate |> Maybe.map Date.toRataDie
    in
    div
        [ classList
            [ ( "date-selector", True )
            , ( "date-selector--scrollable-year", RataDie.year maximum - RataDie.year minimum >= 12 )
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
        |> Html.map (clamp minimum maximum >> Date.fromRataDie)


viewYearList : RataDie -> RataDie -> Maybe RataDie -> Html RataDie
viewYearList minimum maximum maybeSelected =
    let
        isInvertedMinMax =
            minimum > maximum

        years =
            if isInvertedMinMax then
                [ maybeSelected |> Maybe.withDefault minimum |> RataDie.year ]
            else
                List.range (RataDie.year minimum) (RataDie.year maximum)

        isSelectedYear : Int -> Bool
        isSelectedYear =
            maybeSelected
                |> Maybe.map (\selected -> (==) (RataDie.year selected))
                |> Maybe.withDefault (\_ -> False)
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (dateWithYear (maybeSelected |> Maybe.withDefault (RataDie.firstOfYear (RataDie.year minimum))))
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


viewMonthList : RataDie -> RataDie -> RataDie -> Html RataDie
viewMonthList minimum maximum selected =
    let
        isInvertedMinMax =
            minimum > maximum

        first =
            if RataDie.year selected == RataDie.year minimum then
                RataDie.monthNumber minimum
            else
                1

        last =
            if RataDie.year selected == RataDie.year maximum then
                RataDie.monthNumber maximum
            else
                12
    in
    ol
        [ on "click" <|
            Json.Decode.map
                (RataDie.numberToMonth >> dateWithMonth selected)
                (Json.Decode.at [ "target", "data-month" ] Json.Decode.int)
        ]
        (monthNames
            |> List.indexedMap
                (\i name ->
                    let
                        n =
                            i + 1

                        state =
                            if n == RataDie.monthNumber selected then
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


viewDateTable : RataDie -> RataDie -> RataDie -> Html RataDie
viewDateTable minimum maximum selected =
    let
        isInvertedMinMax =
            minimum > maximum

        weeks =
            monthDates (RataDie.year selected) (RataDie.month selected) |> groupsOf 7
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
                                                else if RataDie.monthNumber date /= RataDie.monthNumber selected then
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
                                            [ text (RataDie.day date |> toString) ]
                                    )
                            )
                    )
            )
        ]


viewDateTableDisabled : RataDie -> Html a
viewDateTableDisabled date =
    let
        weeks =
            monthDates (RataDie.year date) (RataDie.month date) |> groupsOf 7

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
                                    [ text (RataDie.day date |> toString) ]
                            )
                            weekdates
                )
                weeks
        ]
