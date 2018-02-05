module Main exposing (..)

import Date.Basic as Date exposing (Date, Month(..))
import DateSelectorDropdown
import Html exposing (Html, div, h1, text)
import String


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            Model
                (Date.fromCalendarDate 2011 Mar 15)
                (Date.fromCalendarDate 2017 Sep 15)
                (Date.fromCalendarDate 2016 Sep 15)
                False
        , view = view
        , update = update
        }



-- model


type alias Model =
    { minimum : Date
    , maximum : Date
    , selected : Date
    , isOpen : Bool
    }



-- update


type Msg
    = Select Date
    | Toggle


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select date ->
            { model | selected = date }

        Toggle ->
            { model | isOpen = not model.isOpen }



-- view


view : Model -> Html Msg
view { minimum, maximum, selected, isOpen } =
    div []
        [ Html.node "style"
            []
            [ text <|
                String.join " "
                    [ "@import url(./examples.css);"
                    , "@import url(./date-selector.css);"
                    ]
            ]
        , h1 [] [ text <| Date.toFormattedString "EEE MMM d, yyyy" selected ]
        , DateSelectorDropdown.view
            Toggle
            Select
            isOpen
            minimum
            maximum
            (Just selected)
        ]
