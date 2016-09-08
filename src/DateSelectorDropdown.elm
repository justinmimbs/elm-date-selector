module DateSelectorDropdown exposing (view, viewWithButton)

{-| UI views for displaying a DateSelector from a dropdown.

## Default Button
@docs view

## Custom Button
@docs viewWithButton
-}

import Date exposing (Date)
import Date.Extra as Date
import DateSelector
import Dropdown
import Html exposing (Html, input)
import Html.App as App
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onClick)


{-| View a button that displays a DateSelector below itself when clicked. The
default button is a read-only text input that displays the selected date in
"yyyy-mm-dd" format.

    DateSelectorDropdown.view
      toggleDropdownMsg
      toSelectDateMsg
      isOpen
      minDate
      maxDate
      maybeSelectedDate
-}
view : msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
view toggle =
  viewWithButton (defaultViewButton toggle) toggle


{-| This function is almost the same as `view`, but it takes one more
argument, a function to create the button view, as its first argument. The
function to create the button view takes two arguments, _isOpen_ and
_maybeSelectedDate_. The custom button is responsible for producing any `msg`
needed to open the dropdown.

    DateSelectorDropdown.viewWithButton
      viewButton
      closeDropdownMsg
      toSelectDateMsg
      isOpen
      minDate
      maxDate
      maybeSelectedDate
-}
viewWithButton : (Bool -> Maybe Date -> Html msg) -> msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
viewWithButton viewButton close toSelect isOpen min max selected =
  let
    dateSelectorView =
      if isOpen then
        Just (DateSelector.view min max selected |> App.map toSelect)
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
