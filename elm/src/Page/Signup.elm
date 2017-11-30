module Page.Signup exposing (Model, ExternalMsg(..), Msg, initialModel, update, view)

import Util exposing ((=>))
import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)


type alias Model =
    { email : String
    , password : String
    , name : String
    }


type Msg
    = Email String
    | Password String
    | SubmitSignup
    | LoginResult (Result Http.Error String)


type ExternalMsg
    = NoOp
    | SetUser User


initialModel : Model
initialModel =
    Model "" "" ""


submitLogin : Model -> Cmd Msg
submitLogin model =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ model.email
    in
        Http.send LoginResult (Http.get url decodeLogin)


decodeLogin : Decode.Decoder String
decodeLogin =
    Decode.at [ "data", "image_url" ] Decode.string


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        Email newEmail ->
            ( { model | email = newEmail }, Cmd.none )
                => NoOp

        Password newPassword ->
            ( { model | password = newPassword }, Cmd.none )
                => NoOp

        SubmitSignup ->
            ( model, submitLogin model )
                => NoOp

        LoginResult (Ok user) ->
            ( { model | name = user }, Cmd.none )
                => NoOp

        LoginResult (Err err) ->
            ( { model | name = toString err }, Cmd.none )
                => NoOp


view : Model -> Html Msg
view model =
    div [ class "login-page" ]
        [ div
            [ class "login-container" ]
            [ h2 [] [ text "Login Form" ]
            , input [ type_ "email", onInput Email, value model.email, placeholder "E-mail" ] []
            , input [ type_ "password", onInput Password, value model.password, placeholder "Password" ] []
            , input [ type_ "submit", value "Sign Up!", onClick SubmitSignup ] []
            , img [ src model.name ] []
            ]
        ]
