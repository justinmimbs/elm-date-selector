module DateSelectorDropdown exposing (view, viewWithButton)

import Date exposing (Date)
import Date.Extra as Date
import DateSelector
import Dropdown
import Html exposing (Html, input)
import Html.App as App
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onClick)


view : msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
view toggle =
  viewWithButton (defaultViewButton toggle) toggle


viewWithButton : (Bool -> Maybe Date -> Html msg) -> msg -> (Date -> msg) -> Bool -> Date -> Date -> Maybe Date -> Html msg
viewWithButton viewButton close mapSelect isOpen min max selected =
  let
    dateSelectorView =
      if isOpen then
        Just (DateSelector.view min max selected |> App.map mapSelect)
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
