module Page.Signup exposing (Model, ExternalMsg(..), Msg, initialModel, update, view)

import Views.Spinner exposing (spinnerIcon)
import Util exposing ((=>))
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , signupMode : Bool
    }


type Msg
    = Email String
    | Password String
    | SignupMode
    | SubmitSignup
    | SignupResponse (Result GQLHttp.Error SignupPayload)
    | UserResponse (Result GQLHttp.Error User)


type ExternalMsg
    = NoOp
    | SetUser SignupPayload


initialModel : Model
initialModel =
    Model "" "" Nothing False True


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

        SignupMode ->
            ( { model | signupMode = not model.signupMode }, Cmd.none )
                => NoOp

        SubmitSignup ->
            { model | error = Nothing, loading = True }
                => Task.attempt SignupResponse (Request.User.signup model.signupMode model)
                => NoOp

        SignupResponse (Ok data) ->
            model
                => Cmd.batch [ storeSession data, Route.modifyUrl Route.Home ]
                => SetUser data

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
                => Cmd.none
                --Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => NoOp

        -- SetUser user
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


view : Model -> Html Msg
view model =
    let
        signupContent =
            if model.loading then
                [ spinnerIcon
                , text " Please Wait..."
                ]
            else if model.signupMode then
                [ text "Sign Up!" ]
            else
                [ text "Login" ]
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
                , modeLinkView model.signupMode
                ]
            ]


modeLinkView : Bool -> Html Msg
modeLinkView signupMode =
    let
        modeText =
            if signupMode then
                "Already have an Account? Sign In"
            else
                "Back to Sign Up"
    in
        div [ class "login-footer" ]
            [ a [ href "javascript:;", onClick SignupMode ]
                [ text modeText ]
            ]


errorView : Maybe String -> Html msg
errorView error =
    case error of
        Just err ->
            div [ class "error" ] [ text err ]

        Nothing ->
            text ""
