module Util exposing ((=>), viewIf)

import Html exposing (Attribute, Html)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        Html.text ""
