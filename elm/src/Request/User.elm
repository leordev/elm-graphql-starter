module Request.User exposing (storeSession, signup, get, listUsers, update, UpdatePayload)

import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GQLHttp
import Util exposing ((=>))
import Request.Helpers exposing (apiUrl)
import Task exposing (Task)
import Data.User exposing (User, UserId(..), userIdToString)
import Ports
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Data.AuthToken as AuthToken exposing (AuthTokenStr(..))


type alias SignupVars =
    { email : String
    , password : String
    }


type alias UpdatePayload =
    { id : String }


storeSession : AuthToken.SignupPayload -> Cmd msg
storeSession data =
    AuthToken.encodeSignupPayload data
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession


userObject =
    object User
        |> with (field "id" [] (map UserId id))
        |> with (field "email" [] string)
        |> with (field "name" [] string)
        |> with (field "bio" [] (nullable string))


usersQuery : Request Query (List User)
usersQuery =
    let
        pageSizeVar =
            Var.optional "pageSize" .pageSize (Var.nullable Var.int) (Just 100)

        queryRoot =
            extract
                (field "allUsers"
                    [ ( "first", Arg.variable pageSizeVar ) ]
                    (list userObject)
                )
    in
        queryRoot
            |> queryDocument
            |> request { pageSize = Nothing }


listUsers : Task GQLHttp.Error (List User)
listUsers =
    GQLHttp.sendQuery apiUrl usersQuery


userQuery : Document Query User { vars | userId : String }
userQuery =
    let
        userIdVar =
            Var.required "id" .userId Var.id

        user =
            userObject

        queryRoot =
            extract
                (field "User"
                    [ ( "id", Arg.variable userIdVar ) ]
                    user
                )
    in
        queryDocument queryRoot


get : UserId -> Task GQLHttp.Error User
get userId =
    let
        req =
            userQuery
                |> request { userId = userIdToString userId }
    in
        GQLHttp.sendQuery apiUrl req


updateMutation : Document Mutation UpdatePayload User
updateMutation =
    let
        nameVar =
            Var.required "name" .name Var.string

        emailVar =
            Var.required "email" .email Var.string

        bioVar =
            Var.optional "bio" .bio Var.string ""

        idVar =
            Var.required "id" (userIdToString << .id) Var.id
    in
        mutationDocument <|
            extract
                (field "updateUser"
                    [ "id" => Arg.variable idVar
                    , "email" => Arg.variable emailVar
                    , "name" => Arg.variable nameVar
                    , "bio" => Arg.variable bioVar
                    ]
                    (object UpdatePayload
                        |> with (field "id" [] string)
                    )
                )


updateMutationRequest : User -> Request Mutation UpdatePayload
updateMutationRequest model =
    updateMutation
        |> request model


update : User -> Task GQLHttp.Error UpdatePayload
update model =
    GQLHttp.sendMutation apiUrl (updateMutationRequest model)


signupMutation : Bool -> Document Mutation AuthToken.SignupPayload SignupVars
signupMutation signupMode =
    let
        mutationName =
            if signupMode then
                "signupUser"
            else
                "authenticateUser"

        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
        mutationDocument <|
            extract
                (field mutationName
                    [ "email" => Arg.variable emailVar
                    , "password" => Arg.variable passwordVar
                    ]
                    (object AuthToken.SignupPayload
                        |> with (field "id" [] (map UserId string))
                        |> with (field "token" [] (map AuthTokenStr string))
                    )
                )


signupMutationRequest : Bool -> { m | email : String, password : String } -> Request Mutation AuthToken.SignupPayload
signupMutationRequest signupMode model =
    signupMutation signupMode
        |> request (SignupVars model.email model.password)


signup : Bool -> { m | email : String, password : String } -> Task GQLHttp.Error AuthToken.SignupPayload
signup signupMode model =
    GQLHttp.sendMutation apiUrl (signupMutationRequest signupMode model)
