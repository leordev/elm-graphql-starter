module Data.Session exposing (Session, attempt)

import Data.AuthToken exposing (AuthTokenStr(..), SignupPayload)
import Data.User as User exposing (User)
import Util exposing ((=>))


type alias Session =
    { user : Maybe User
    , auth : Maybe SignupPayload
    }


attempt : String -> (AuthTokenStr -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case Maybe.map .token session.auth of
        Nothing ->
            [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] => Cmd.none

        Just token ->
            [] => toCmd token
