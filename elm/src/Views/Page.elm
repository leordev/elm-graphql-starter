module Views.Page exposing (ActivePage(..), frame, rawFrame)

import Data.User as User exposing (User, UserId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Util exposing ((=>))
import Views.Spinner exposing (spinnerIcon)


type ActivePage
    = Other
    | Home
    | Users
    | Signup
    | Profile UserId
    | Place


frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , content
        , viewFooter
        ]


rawFrame : Html msg -> Html msg
rawFrame content =
    content


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ a [ class "navbar-brand", Route.href Route.Home ]
            [ text "starter" ]
        , ul []
            [ lazy2 Util.viewIf isLoading spinnerIcon
            , viewWelcomeUser user
            , navbarLink page Route.Home [ text "Home" ]
            , navbarLink page Route.Users [ text "Users" ]
            , viewUserMenu page user
            , navbarLink page Route.Logout [ text "Sign Out" ]
            ]
        ]


viewWelcomeUser : Maybe User -> Html msg
viewWelcomeUser user =
    case user of
        Nothing ->
            li [] [ text "Loading User..." ]

        Just user ->
            li [] [ text ("Hello, " ++ user.name ++ "!") ]


viewUserMenu : ActivePage -> Maybe User -> Html msg
viewUserMenu page user =
    case user of
        Nothing ->
            text ""

        Just user ->
            navbarLink page (Route.Profile user.id) [ text "Profile" ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ class "logo-font", href "/" ] [ text "starter" ]
        , span [ class "attribution" ]
            [ text "Just a Starter Bootstrap for Elm + Graphql developed by "
            , a [ href "https://github.com/leordev" ] [ text "leordev" ]
            , text ". Code licensed under MIT."
            ]
        ]


navbarLink : ActivePage -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


isActive : ActivePage -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Signup, Route.Signup ) ->
            True

        ( Profile pageUsername, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        _ ->
            False
