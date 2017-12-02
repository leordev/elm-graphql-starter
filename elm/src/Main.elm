module Main exposing (main)

import Util exposing ((=>))
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Value)
import Html exposing (..)
import Route exposing (Route)
import Page.Errored as Errored exposing (PageLoadError)
import Page.Signup as Signup exposing (Model)
import Page.NotFound as NotFound
import Views.Page as Page exposing (ActivePage)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User, UserId)
import Data.AuthToken as AuthToken exposing (SignupPayload)
import Ports


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home
    | Signup Signup.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type alias Model =
    { session : Session
    , pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = { user = Nothing, auth = decodeAuthFromJson val }
        }


initialPage : Page
initialPage =
    Blank


decodeUserFromJson : Value -> Maybe User
decodeUserFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString User.decoder >> Result.toMaybe)


decodeAuthFromJson : Value -> Maybe SignupPayload
decodeAuthFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString AuthToken.decoder >> Result.toMaybe)



-- UPDATE


type Msg
    = SetRoute (Maybe Route)
    | SignupLoaded (Result PageLoadError Signup.Model)
    | SetUser (Maybe SignupPayload)
    | SignupMsg Signup.Msg


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        errored =
            pageErrored model

        log =
            Debug.log ">>>>> setRoute : route" maybeRoute
    in
        case maybeRoute of
            Nothing ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just Route.Signup ->
                ( { model | pageState = Loaded (Signup Signup.initialModel) }, Cmd.none )

            Just Route.Home ->
                case model.session.auth of
                    Just auth ->
                        ( { model | pageState = Loaded Home }, Cmd.none )

                    Nothing ->
                        errored Page.Home "You must be signed in to view content."

            Just Route.Logout ->
                let
                    session =
                        model.session
                in
                    ( { model | session = { session | user = Nothing, auth = Nothing } }
                    , Cmd.batch
                        [ Ports.storeSession Nothing
                        , Route.modifyUrl Route.Signup
                        ]
                    )

            Just (Route.Profile id) ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )

            Just (Route.Place slug) ->
                ( { model | pageState = Loaded NotFound }, Cmd.none )


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        ( { model | pageState = Loaded (Errored error) }, Cmd.none )


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        errored =
            pageErrored model

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( SignupLoaded (Ok subModel), _ ) ->
                ( { model | pageState = Loaded (Signup subModel) }, Cmd.none )

            ( SignupMsg subMsg, Signup subModel ) ->
                let
                    ( ( signupModel, cmd ), msgFromSignup ) =
                        Signup.update subMsg subModel

                    newModel =
                        case msgFromSignup of
                            Signup.NoOp ->
                                model

                            Signup.SetUser auth ->
                                let
                                    session =
                                        model.session

                                    newSession =
                                        { session | auth = Just auth }
                                in
                                    { model | session = newSession }
                in
                    { newModel | pageState = Loaded (Signup signupModel) }
                        => Cmd.map SignupMsg cmd

            ( _, NotFound ) ->
                ( model, Cmd.none )

            ( _, _ ) ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        , Sub.map SetUser sessionChange
        ]


sessionChange : Sub (Maybe SignupPayload)
sessionChange =
    Ports.onSessionChange (Decode.decodeValue AuthToken.decoder >> Result.toMaybe)


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        Signup subModel ->
            Sub.none

        Home ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        frame =
            Page.frame isLoading session.user
    in
        case page of
            NotFound ->
                NotFound.view session
                    |> frame Page.Other

            Blank ->
                -- This is for the very initial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Errored subModel ->
                Errored.view session subModel
                    |> frame Page.Other

            Signup subModel ->
                Signup.view subModel
                    |> Page.rawFrame
                    |> Html.map SignupMsg

            Home ->
                NotFound.view session
                    |> frame Page.Other
