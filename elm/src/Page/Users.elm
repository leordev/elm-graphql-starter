module Page.Users exposing (view, Model, init)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text, p, ul, li)
import Html.Attributes exposing (alt, class, id, src, tabindex, style)
import Util exposing ((=>))
import Data.User exposing (User)
import GraphQL.Client.Http as GQLHttp
import Task exposing (Task)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Request.User


-- MODEL --


type alias Model =
    { users : List User }


init : Session -> Task PageLoadError Model
init session =
    let
        handleLoadError _ =
            pageLoadError Page.Other "User list is currently unavailable."
    in
        Task.map Model Request.User.listUsers
            |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = UsersResponse (Result GQLHttp.Error User)



-- VIEW --


view : Session -> Model -> Html msg
view session model =
    main_ [ id "content", class "container", tabindex -1 ]
        [ h1 [] [ text "Users" ]
        , ul [] (List.map viewUser model.users)
        ]


viewUser : User -> Html msg
viewUser user =
    li [] [ text user.name ]
