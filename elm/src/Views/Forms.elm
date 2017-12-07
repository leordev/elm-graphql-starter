module Views.Forms exposing (inputText)

import Html exposing (Html, Attribute, div, input, label, text)
import Html.Attributes exposing (value, style)
import Util exposing ((=>))


inputText : Bool -> String -> String -> List (Attribute msg) -> Html msg
inputText edit labelText val attr =
    div [ style [ "margin-bottom" => "14px" ] ]
        [ label [] [ text labelText ]
        , viewContent edit val attr
        ]


viewContent : Bool -> String -> List (Attribute msg) -> Html msg
viewContent edit val attrs =
    if edit then
        input
            ([ value val ] ++ attrs)
            []
    else if val == "" then
        text "(empty)"
    else
        text val
