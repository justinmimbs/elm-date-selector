module Main exposing (..)

import Date.Basic as Date exposing (Date, Month(..))
import DateSelector
import Html exposing (Html, div, h1, text)
import String


main : Program Never Model Date
main =
    Html.beginnerProgram
        { model =
            Model
                (Date.fromCalendarDate 2011 Mar 15)
                (Date.fromCalendarDate 2017 Sep 15)
                (Date.fromCalendarDate 2016 Sep 15)
        , view = view
        , update = update
        }



-- model


type alias Model =
    { minimum : Date
    , maximum : Date
    , selected : Date
    }



-- update


update : Date -> Model -> Model
update date model =
    { model | selected = date }



-- view


view : Model -> Html Date
view { minimum, maximum, selected } =
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
        , DateSelector.view minimum maximum (Just selected)
        ]
