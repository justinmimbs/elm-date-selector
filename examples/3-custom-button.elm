module Example3 exposing (main)

import Browser
import Date exposing (Date)
import DateSelectorDropdown
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Time exposing (Month(..))


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
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
    Html.div []
        [ Html.node "style"
            []
            [ Html.text <|
                String.join " "
                    [ "@import url(./examples.css);"
                    , "@import url(./date-selector.css);"
                    ]
            ]
        , Html.h1 [] [ Html.text (selected |> Maybe.map (Date.format "'Just' (EEE MMM d, yyyy)") |> Maybe.withDefault "Nothing") ]
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
    Html.div
        [ Html.Attributes.classList
            [ ( "date-selector-dropdown-button", True )
            , ( "date-selector-dropdown-button--open", isOpen )
            ]
        ]
        [ Html.div
            [ Html.Attributes.class "date-selector-dropdown-button--date" ]
            [ Html.text (selected |> Maybe.map (Date.format "yyyy MMM d") |> Maybe.withDefault "") ]
        , case selected of
            Just _ ->
                Html.div
                    [ Html.Attributes.class "date-selector-dropdown-button--clear"
                    , Html.Events.onClick (Select Nothing)
                    ]
                    [ Html.text "✕" ]

            Nothing ->
                Html.div
                    [ Html.Attributes.class "date-selector-dropdown-button--clear" ]
                    []
        , Html.div
            [ Html.Attributes.class "date-selector-dropdown-button--toggle"
            , Html.Events.onClick Toggle
            ]
            [ Html.span []
                [ Html.text <|
                    if isOpen then
                        "▲"

                    else
                        "▼"
                ]
            ]
        ]
