module Page.Signup exposing (Model, ExternalMsg(..), Msg, initialModel, update, view)

import Views.Spinner exposing (spinnerIcon)
import Util exposing ((=>))
import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, UserId(..))
import Route exposing (Route)
import Task
import Request.User exposing (storeSession)
import Data.AuthToken exposing (SignupPayload)
import GraphQL.Client.Http as GQLHttp


type alias Model =
    { email : String
    , password : String
    , error : Maybe String
    , loading : Bool
    }


type Msg
    = Email String
    | Password String
    | SubmitSignup
    | LoginResult (Result Http.Error String)
    | SignupResponse (Result GQLHttp.Error SignupPayload)
    | UserResponse (Result GQLHttp.Error User)


type ExternalMsg
    = NoOp
    | SetUser User


initialModel : Model
initialModel =
    Model "" "" Nothing False


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
            { model | error = Nothing, loading = True }
                => Task.attempt SignupResponse (Request.User.signup model)
                => NoOp

        SignupResponse (Ok data) ->
            model
                => Task.attempt UserResponse (Request.User.get (UserId data.id))
                => NoOp

        SignupResponse (Err err) ->
            let
                errorMessage =
                    case err of
                        GQLHttp.GraphQLError gqlErr ->
                            case (List.head gqlErr) of
                                Just gqlErrMsg ->
                                    gqlErrMsg.message

                                Nothing ->
                                    "Error while Signing Up"

                        GQLHttp.HttpError httpErr ->
                            toString httpErr
            in
                { model | loading = False, error = Just errorMessage }
                    => Cmd.none
                    => NoOp

        UserResponse (Ok user) ->
            { model | error = Nothing, loading = False }
                => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => SetUser user

        UserResponse (Err err) ->
            let
                errorMessage =
                    case err of
                        GQLHttp.GraphQLError gqlErr ->
                            case (List.head gqlErr) of
                                Just gqlErrMsg ->
                                    gqlErrMsg.message

                                Nothing ->
                                    "Error while Signing Up"

                        GQLHttp.HttpError httpErr ->
                            toString httpErr
            in
                { model | loading = False, error = Just errorMessage }
                    => Cmd.none
                    => NoOp

        LoginResult (Ok user) ->
            ( { model | error = Just user }, Cmd.none )
                => NoOp

        LoginResult (Err err) ->
            ( { model | error = Just (toString err) }, Cmd.none )
                => NoOp


view : Model -> Html Msg
view model =
    let
        signupContent =
            if model.loading then
                [ spinnerIcon
                , text " Please Wait..."
                ]
            else
                [ text "Sign Up!" ]
    in
        div [ class "login-page" ]
            [ div
                [ class "login-container" ]
                [ h1 [] [ text "Login Form" ]
                , input [ type_ "email", onInput Email, value model.email, placeholder "E-mail" ] []
                , input [ type_ "password", onInput Password, value model.password, placeholder "Password" ] []
                , button
                    [ disabled model.loading
                    , onClick SubmitSignup
                    ]
                    signupContent
                , errorView model.error
                ]
            ]


errorView : Maybe String -> Html msg
errorView error =
    case error of
        Just err ->
            div [ class "error" ] [ text err ]

        Nothing ->
            text ""
