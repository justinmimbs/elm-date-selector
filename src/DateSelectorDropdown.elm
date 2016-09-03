module DateSelectorDropdown exposing (view, viewWithButton)

import Date exposing (Date)
import Date.Extra as Date
import DateSelector
import Dropdown
import Html exposing (Html, input)
import Html.App as App
import Html.Attributes exposing (class, readonly, value)


view : msg -> (Date -> msg) -> Bool -> Date -> Date -> Date -> Html msg
view =
  viewWithButton defaultViewButton


viewWithButton : (Bool -> Date -> Html a) -> msg -> (Date -> msg) -> Bool -> Date -> Date -> Date -> Html msg
viewWithButton viewButton toggle mapSelect isOpen min max selected =
  let
    dateSelectorView =
      if isOpen then
        Just (DateSelector.view min max selected |> App.map mapSelect)
      else
        Nothing
  in
    Dropdown.view
      toggle
      (viewButton isOpen selected)
      dateSelectorView


defaultViewButton : Bool -> Date -> Html a
defaultViewButton isOpen date =
  input
    [ class "date-selector-dropdown--input"
    , readonly True
    , value <| Date.toFormattedString "yyyy-MM-dd" date
    ]
    []
