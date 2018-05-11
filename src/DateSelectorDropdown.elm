module DateSelectorDropdown exposing (view, viewWithButton)

{-| Create a button that displays a `DateSelector` view below itself when
clicked.


## Default Button

@docs view


## Custom Button

@docs viewWithButton

-}

import Date.Basic as Date exposing (Date)
import DateSelector
import Dropdown
import Html exposing (Html, input)
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onClick)


{-| The default button is a read-only text input that displays the selected
date in "yyyy-mm-dd" format.

The first argument is the message you want to receive to toggle the dropdown;
the second argument constructs a message from the user-selected date. The third
argument indicates whether or not the `DateSelector` view should be displayed.
The final three arguments are used to create the `DateSelector` view if
necessary.

    DateSelectorDropdown.view
        toggleDropdownMsg
        toSelectDateMsg
        isOpen
        minimum
        maximum
        selected

-}
view : msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
view toggle =
    viewWithButton (defaultViewButton toggle) toggle


{-| This function is almost the same as `view`, but takes, as its first
argument, the function to create the custom button. The function to create the
custom button will receive _isOpen_ and _selected_, and the `Html` it creates
is responsible for producing any `msg` needed to open the dropdown.

    DateSelectorDropdown.viewWithButton
        viewButton
        closeDropdownMsg
        toSelectDateMsg
        isOpen
        minimum
        maximum
        selected

A full example is available [here](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/3-custom-button.elm).

-}
viewWithButton : (Bool -> Maybe Date -> Html msg) -> msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
viewWithButton viewButton close toSelect isOpen minimum maximum selected =
    let
        dateSelectorView =
            if isOpen then
                Just (DateSelector.view minimum maximum selected |> Html.map toSelect)
            else
                Nothing
    in
    Dropdown.view
        close
        (viewButton isOpen selected)
        dateSelectorView


defaultViewButton : msg -> Bool -> Maybe Date -> Html msg
defaultViewButton toggle isOpen maybeDate =
    input
        [ value (maybeDate |> Maybe.map (Date.toFormattedString "yyyy-MM-dd") |> Maybe.withDefault "")
        , readonly True
        , onClick toggle
        ]
        []
