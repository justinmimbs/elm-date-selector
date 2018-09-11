module Dropdown exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed


view : msg -> Html msg -> Maybe (Html msg) -> Html msg
view close button maybeContent =
    let
        buttonContainer =
            Html.div
                [ Html.Attributes.class "dropdown--button-container" ]
                [ button ]
    in
    case maybeContent of
        Nothing ->
            Html.Keyed.node "div"
                [ Html.Attributes.class "dropdown" ]
                [ ( "button"
                  , buttonContainer
                  )
                ]

        Just content ->
            Html.Keyed.node "div"
                [ Html.Attributes.class "dropdown-open" ]
                [ ( "cover"
                  , Html.div
                        [ Html.Attributes.class "dropdown--page-cover"
                        , Html.Events.onClick close
                        ]
                        []
                  )
                , ( "button"
                  , buttonContainer
                  )
                , ( "content"
                  , Html.div
                        [ Html.Attributes.class "dropdown--content-container" ]
                        [ content ]
                  )
                ]
