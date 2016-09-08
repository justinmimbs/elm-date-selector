# elm-date-selector

The `DateSelector` view displays a familiar calendar for selecting a date, but allows a user to select the year, month, and day independently. This allows for quick selection of a date without changing modes or stepping through calendars one month at a time.

The core view is provided in the `DateSelector` module, and a dropdown view is available in `DateSelectorDropdown`. Both modules expose only view functions. Because there is little data to manage, how you store and update the data is up to you.

## DateSelector

![DateSelector](http://justinmimbs.com/etcetera/date-selector.png)

The `DateSelector.view` function requires only `Date` values (for the _minimum_, _maximum_, and _selected_ dates) and creates `Html` that produces `Date` messages; a side-effect of the UI design is that no "internal" state is required. You can map its `Date` messages to your own message type.

```elm
type Msg = SelectDate Date | ...

view : Html Msg
view =
  DateSelector.view minDate maxDate (Just selectedDate)
    |> Html.App.map SelectDate
```

## DateSelectorDropdown

![DateSelectorDropdown](http://justinmimbs.com/etcetera/date-selector-dropdown.png)

To use the `DateSelectorDropdown.view` function, the only additional state you must provide is a `Bool` indicating whether or not the dropdown is open. The function also takes two message constructors so that you can receive appropriate messages when a user wants to toggle the dropdown and when they want to select a date. 

The dropdown can be displayed from a custom button using the `viewWithButton` function. See the [docs]() and the examples listed below for more detail.

## Examples

  1. [Standalone DateSelector]()
  2. [DateSelectorDropdown]()
  3. [DateSelectorDropdown with custom button]()
  4. [Multiple DateSelectorDropdowns]()

## CSS

The styles are available as a CSS file [here](https://github.com/justinmimbs/elm-date-selector/examples/date-selector-dropdown.css). This file does not declare a font-family, and all sizes are specified in em units, allowing font styles to be inherited. Similarly, the default button for `DateSelectorDropdown.view` is an unstyled `<input>`, allowing its look to be inherited.
