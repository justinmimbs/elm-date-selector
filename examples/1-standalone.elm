module Example1 exposing (main)

import Browser
import Date exposing (Date)
import DateSelector
import Html exposing (Html)
import Time exposing (Month(..))


main : Program () Model Date
main =
    Browser.sandbox
        { init =
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
    Html.div []
        [ Html.node "style"
            []
            [ Html.text <|
                String.join " "
                    [ "@import url(./examples.css);"
                    , "@import url(./date-selector.css);"
                    ]
            ]
        , Html.h1 [] [ Html.text <| Date.format "EEE MMM d, yyyy" selected ]
        , DateSelector.view minimum maximum (Just selected)
        ]
