module Data.User exposing (User, userIdToString, UserId(..), decoder, encode, userIdParser)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import UrlParser


type UserId
    = UserId String


type alias User =
    { id : UserId
    , email : String
    , name : String
    , bio : Maybe String
    }


userIdToString : UserId -> String
userIdToString (UserId id) =
    id


userIdParser : UrlParser.Parser (UserId -> a) a
userIdParser =
    UrlParser.custom "USERID" (Ok << UserId)



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "id" (Decode.map UserId Decode.string)
        |> required "email" Decode.string
        |> required "name" Decode.string
        |> required "bio" (Decode.nullable Decode.string)


encode : User -> Value
encode user =
    Encode.object
        [ "email" => Encode.string user.email
        , "id" => Encode.string (userIdToString user.id)
        , "name" => Encode.string user.name
        , "bio" => EncodeExtra.maybe Encode.string user.bio
        ]
