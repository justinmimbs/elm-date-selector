# elm-date-selector

The `DateSelector` view displays a familiar calendar for selecting a date, but allows a user to select the year, month, and day independently. This allows for quick selection of a date without changing modes or stepping through calendars one month at a time.

The core view is provided in the `DateSelector` module, and a dropdown view is available in `DateSelectorDropdown`. Both modules expose only view functions. Because there is little data to manage, how you store and update the data is up to you.

## DateSelector

![DateSelector](http://justinmimbs.com/etcetera/date-selector.png)

The `DateSelector.view` function takes only `Date` values (for the _minimum_, _maximum_, and _selected_ dates; a side-effect of the UI design is that no "internal" state is needed) and creates `Html` that produces `Date` messages. You can map its `Date` messages to your own message type.

```elm
type Msg = SelectDate Date | ...

view : Html Msg
view =
  DateSelector.view minDate maxDate (Just selectedDate)
    |> Html.map SelectDate
```

## DateSelectorDropdown

![DateSelectorDropdown](http://justinmimbs.com/etcetera/date-selector-dropdown.png)

The `DateSelectorDropdown.view` function creates a button and a `DateSelector` view when it's open. You must provide a `Bool` indicating whether or not the dropdown is open, as well as constructors for the messages you want to receive when a user toggles the dropdown and when they select a date.

The `DateSelectorDropdown.viewWithButton` function allows you to use your own button with the dropdown. See the [docs](http://package.elm-lang.org/packages/justinmimbs/elm-date-selector/latest/DateSelectorDropdown) and the examples listed below for more detail.

## Examples

  1. [Standalone DateSelector](https://justinmimbs.github.io/elm-date-selector/1-standalone.html) / [source](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/1-standalone.elm)
  2. [DateSelectorDropdown](https://justinmimbs.github.io/elm-date-selector/2-dropdown.html) / [source](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/2-dropdown.elm)
  3. [DateSelectorDropdown with custom button](https://justinmimbs.github.io/elm-date-selector/3-custom-button.html) / [source](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/3-custom-button.elm)
  4. [Multiple DateSelectorDropdowns](https://justinmimbs.github.io/elm-date-selector/4-multiple-dropdowns.html) / [source](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/4-multiple-dropdowns.elm)

## CSS

The styles are available as a CSS file [here](https://github.com/justinmimbs/elm-date-selector/blob/master/examples/date-selector.css). This file does not declare a font-family, and all sizes are specified in em units, allowing font styles to be inherited. Similarly, the default button for `DateSelectorDropdown.view` is an unstyled `<input>`, allowing its look to be inherited.
