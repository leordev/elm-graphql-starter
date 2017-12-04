module Page.Home exposing (view)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text, p)
import Html.Attributes exposing (alt, class, id, src, tabindex, style)
import Util exposing ((=>))


-- VIEW --


view : Session -> Html msg
view session =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Home" ]
        , p [] [ text "Welcome to starter! It's just a SPA or PWA to start building apps with Elm utilizing Graphql!" ]
        ]
