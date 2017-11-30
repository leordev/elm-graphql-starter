module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)
import Data.Place as Place
import Data.User as User


type Route
    = Home
    | Logout
    | Signup
    | Profile User.UserId
    | Place Place.Slug


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "home")
        , Url.map Logout (s "logout")
        , Url.map Signup (s "")
        , Url.map Place (s "place" </> Place.slugParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    [ "home" ]

                Logout ->
                    [ "logout" ]

                Signup ->
                    []

                Place slug ->
                    [ "place", Place.slugToString slug ]

                Profile id ->
                    [ "profile", User.userIdToString id ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Signup
    else
        parseHash route location
