module Main exposing (..)

import Date exposing (Date, Month(..))
import Date.Extra as Date
import DateSelectorDropdown
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import String


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            Model
                (Date.fromCalendarDate 2011 Mar 15)
                (Date.fromCalendarDate 2017 Sep 15)
                (Just <| Date.fromCalendarDate 2016 Sep 15)
                False
        , view = view
        , update = update
        }



-- model


type alias Model =
    { minimum : Date
    , maximum : Date
    , selected : Maybe Date
    , isOpen : Bool
    }



-- update


type Msg
    = Select (Maybe Date)
    | Toggle


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select maybeDate ->
            { model | selected = maybeDate }

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
        , h1 [] [ text (selected |> Maybe.map (Date.toFormattedString "'Just' (EEE MMM d, yyyy)") |> Maybe.withDefault "Nothing") ]
        , DateSelectorDropdown.viewWithButton
            viewCustomButton
            Toggle
            (Select << Just)
            isOpen
            minimum
            maximum
            selected
        ]


viewCustomButton : Bool -> Maybe Date -> Html Msg
viewCustomButton isOpen selected =
    div
        [ classList
            [ ( "date-selector-dropdown-button", True )
            , ( "date-selector-dropdown-button--open", isOpen )
            ]
        ]
        [ div
            [ class "date-selector-dropdown-button--date" ]
            [ text (selected |> Maybe.map (Date.toFormattedString "yyyy MMM d") |> Maybe.withDefault "") ]
        , case selected of
            Just _ ->
                div
                    [ class "date-selector-dropdown-button--clear"
                    , onClick (Select Nothing)
                    ]
                    [ text "✕" ]

            Nothing ->
                div
                    [ class "date-selector-dropdown-button--clear" ]
                    []
        , div
            [ class "date-selector-dropdown-button--toggle"
            , onClick Toggle
            ]
            [ span []
                [ text <|
                    if isOpen then
                        "▲"
                    else
                        "▼"
                ]
            ]
        ]
