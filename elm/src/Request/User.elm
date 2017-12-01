module Request.User exposing (storeSession, signup, get)

import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GQLHttp
import Util exposing ((=>))
import Request.Helpers exposing (apiUrl)
import Task exposing (Task)
import Data.User exposing (User, UserId(..), userIdToString, encode)
import Ports
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Data.AuthToken as AuthToken exposing (AuthToken(..))


type alias SignupVars =
    { email : String
    , password : String
    }


storeSession : User -> Cmd msg
storeSession user =
    encode user
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession


userQuery : Document Query User { vars | userId : String }
userQuery =
    let
        userIdVar =
            Var.required "id" .userId Var.id

        user =
            object User
                |> with (field "id" [] (map UserId id))
                |> withLocalConstant (AuthToken "zzz")
                |> with (field "email" [] string)
                |> with (field "name" [] string)
                |> with (field "bio" [] (nullable string))

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


signupMutation : Document Mutation AuthToken.SignupPayload SignupVars
signupMutation =
    let
        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
        mutationDocument <|
            extract
                (field "signupUser"
                    [ "email" => Arg.variable emailVar
                    , "password" => Arg.variable passwordVar
                    ]
                    (object AuthToken.SignupPayload
                        |> with (field "id" [] string)
                        |> with (field "token" [] string)
                    )
                )


signupMutationRequest : { m | email : String, password : String } -> Request Mutation AuthToken.SignupPayload
signupMutationRequest model =
    signupMutation
        |> request (SignupVars model.email model.password)


signup : { m | email : String, password : String } -> Task GQLHttp.Error AuthToken.SignupPayload
signup model =
    GQLHttp.sendMutation apiUrl (signupMutationRequest model)
