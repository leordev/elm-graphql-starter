module Views.Forms exposing (inputText, inputPassword)

import Html exposing (Html, Attribute, div, input, label, text)
import Html.Attributes exposing (value, style, type_)
import Util exposing ((=>))


type InputType
    = Text
    | Password


inputText : Bool -> String -> String -> List (Attribute msg) -> Html msg
inputText edit labelText val attr =
    inputTypes Text edit labelText val attr


inputPassword : Bool -> String -> String -> List (Attribute msg) -> Html msg
inputPassword edit labelText val attr =
    inputTypes Password edit labelText val attr


inputTypes : InputType -> Bool -> String -> String -> List (Attribute msg) -> Html msg
inputTypes inputType edit labelText val attr =
    div [ style [ "margin-bottom" => "14px" ] ]
        [ label [] [ text labelText ]
        , viewInput inputType edit val attr
        ]


viewInput : InputType -> Bool -> String -> List (Attribute msg) -> Html msg
viewInput inputTypeParam edit val attrs =
    let
        inputType =
            case inputTypeParam of
                Text ->
                    "text"

                Password ->
                    "password"
    in
        if edit then
            input
                ([ type_ inputType, value val ] ++ attrs)
                []
        else if val == "" then
            text "(empty)"
        else
            text val
