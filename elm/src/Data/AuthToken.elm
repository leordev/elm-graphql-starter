module Data.AuthToken exposing (AuthTokenStr(..), decoder, encode, withAuthorization, SignupPayload, encodeSignupPayload)

import HttpBuilder exposing (RequestBuilder, withHeader)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type AuthTokenStr
    = AuthTokenStr String


type alias SignupPayload =
    { id : String
    , token : AuthTokenStr
    }


encode : AuthTokenStr -> Value
encode (AuthTokenStr token) =
    Encode.string token


authTokenToString : AuthTokenStr -> String
authTokenToString (AuthTokenStr token) =
    token


encodeSignupPayload : SignupPayload -> Value
encodeSignupPayload { id, token } =
    Encode.object
        [ "id" => Encode.string id
        , "token" => Encode.string (authTokenToString token)
        ]


decoder : Decoder SignupPayload
decoder =
    decode SignupPayload
        |> required "id" Decode.string
        |> required "token" (Decode.map AuthTokenStr Decode.string)


withAuthorization : Maybe AuthTokenStr -> RequestBuilder a -> RequestBuilder a
withAuthorization maybeToken builder =
    case maybeToken of
        Just (AuthTokenStr token) ->
            builder
                |> withHeader "authorization" ("Bearer " ++ token)

        Nothing ->
            builder
