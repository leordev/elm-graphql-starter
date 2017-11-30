module Page.NotFound exposing (view)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)


-- VIEW --


view : Session -> Html msg
view session =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Not Found" ]
        , div [ class "row" ]
            [ img [ src "https://memegenerator.net/img/instances/20590351/404-page-not-found-okay.jpg", alt "giant laser walrus wreaking havoc" ] [] ]
        ]
