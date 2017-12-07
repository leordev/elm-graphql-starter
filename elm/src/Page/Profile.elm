module Page.Profile exposing (view, Model, init, Msg, update)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, img, main_, text, p, ul, li, button)
import Html.Attributes exposing (alt, class, id, src, tabindex, style)
import Html.Events exposing (onInput, onClick)
import Util exposing ((=>))
import Data.User exposing (User, UserId(..))
import GraphQL.Client.Http as GQLHttp
import Task exposing (Task)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Views.Page as Page
import Request.User
import Views.Forms as Forms


-- MODEL --


type alias Model =
    { profile : User
    , edit : Bool
    , email : String
    , name : String
    , bio : String
    , password : String
    }


init : Session -> UserId -> Task PageLoadError Model
init session id =
    let
        handleLoadError _ =
            pageLoadError Page.Other "Profile page is currently unavailable"
    in
        Task.map initModel (Request.User.get id)
            |> Task.mapError handleLoadError


initModel : User -> Model
initModel user =
    { profile = user
    , edit = False
    , email = user.email
    , name = user.name
    , bio = Maybe.withDefault "" user.bio
    , password = ""
    }



-- UPDATE --


type Msg
    = ProfileResponse (Result GQLHttp.Error User)
    | SetEditMode
    | SetEmail String
    | SetBio String
    | SetName String
    | SetPassword String
    | SaveProfile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEmail email ->
            { model | email = email } => Cmd.none

        SetBio bio ->
            { model | bio = bio } => Cmd.none

        SetEditMode ->
            { model | edit = not model.edit } => Cmd.none

        _ ->
            model => Cmd.none



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        userId =
            Maybe.map .id session.auth

        title =
            if model.edit then
                "Editing Profile"
            else
                model.profile.name
    in
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text title ]
            , viewProfile model userId
            ]


viewProfile : Model -> Maybe UserId -> Html Msg
viewProfile model authId =
    let
        canEdit =
            case authId of
                Just id ->
                    model.profile.id == id

                Nothing ->
                    False

        editButton =
            if canEdit && not model.edit then
                button [ onClick SetEditMode ] [ text "Edit Profile" ]
            else
                text ""

        saveButton =
            if model.edit then
                button [ onClick SaveProfile ] [ text "Submit" ]
            else
                text ""

        nameField =
            if model.edit then
                Forms.inputText edit "Name" model.name [ onInput SetName ]
            else
                text ""

        passwordField =
            if model.edit then
                Forms.inputText edit
                    "Password (leave it blank if you don't want to change it)"
                    model.password
                    [ onInput SetPassword ]
            else
                text ""

        edit =
            model.edit
    in
        div []
            [ nameField
            , Forms.inputText edit "Email" model.email [ onInput SetEmail ]
            , Forms.inputText edit "Bio" model.bio [ onInput SetBio ]
            , passwordField
            , saveButton
            , editButton
            ]
