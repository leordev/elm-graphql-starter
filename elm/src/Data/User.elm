module Data.User exposing (User, userIdToString, UserId, decoder)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)


type UserId
    = UserId String


type alias User =
    { id : UserId
    , token : AuthToken
    , email : String
    , name : String
    , bio : Maybe String
    }


userIdToString : UserId -> String
userIdToString (UserId id) =
    id



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "id" (Decode.map UserId Decode.string)
        |> required "token" AuthToken.decoder
        |> required "email" Decode.string
        |> required "name" Decode.string
        |> required "bio" (Decode.nullable Decode.string)
