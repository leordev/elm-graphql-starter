module Views.Spinner exposing (spinner, spinnerIcon)

import Html exposing (Attribute, Html, div, li, span)
import Html.Attributes exposing (class, style)
import Util exposing ((=>))


spinner : Html msg
spinner =
    li [ class "sk-three-bounce", style [ "float" => "left", "margin" => "8px" ] ]
        [ div [ class "sk-child sk-bounce1" ] []
        , div [ class "sk-child sk-bounce2" ] []
        , div [ class "sk-child sk-bounce3" ] []
        ]


spinnerIcon : Html msg
spinnerIcon =
    span [ class "sk-three-bounce", style [ "float" => "left", "margin" => "0 8px" ] ]
        [ div [ class "sk-child sk-bounce1" ] []
        , div [ class "sk-child sk-bounce2" ] []
        , div [ class "sk-child sk-bounce3" ] []
        ]
