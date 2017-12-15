module Main exposing (main)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Text as Text
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Tab as Tab
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Files
import Login
import Settings
import Http
import Api
import UrlParser exposing (..)


main : Program Never State Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type Msg
    = LoginMsg Login.Msg
    | FilesMsg Files.Msg
    | SettingsMsg Settings.Msg
    | UrlChange Navigation.Location
    | LoginAttemptResult (Result Http.Error (Maybe Api.User))
    | LogoutResult (Result Http.Error ())


type Location
    = LoginLoc
    | FilesLoc (Maybe Int) Bool Files.Sort Files.SortedColumn
    | SettingsLoc
    | LogoutLoc


type Page
    = Login Login.Model
    | Files Files.Model
    | Settings Settings.Model
    | NotFound


type State
    = BeforeLoggingInAttempt Navigation.Location
    | AfterLoggingInAttempt Model


type alias Model =
    { page : Page
    , user : Maybe Api.User
    }


getUser : Page -> Maybe Api.User
getUser page =
    case page of
        Settings model ->
            Just model.user

        Files model ->
            Just model.user

        _ ->
            Nothing


locParser : Parser (Location -> a) a
locParser =
    let
        isJust m =
            case m of
                Nothing ->
                    False

                Just _ ->
                    True
    in
        oneOf
            [ UrlParser.map LoginLoc UrlParser.top
            , UrlParser.map SettingsLoc (UrlParser.s "settings")
            , UrlParser.map LogoutLoc (UrlParser.s "logout")
            , UrlParser.map
                (\i maybeError sortS sortedColumnS ->
                    FilesLoc (Just i)
                        (isJust maybeError)
                        (case sortS of
                            Just "desc" ->
                                Files.Desc

                            _ ->
                                Files.Asc
                        )
                        (case sortedColumnS of
                            Just "date" ->
                                Files.Date

                            _ ->
                                Files.Name
                        )
                )
                (UrlParser.s "main" </> int <?> intParam "error" <?> stringParam "sort" <?> stringParam "sortedColumn")
            , UrlParser.map
                (\maybeError sortS sortedColumnS ->
                    FilesLoc Nothing
                        (isJust maybeError)
                        (case sortS of
                            Just "desc" ->
                                Files.Desc

                            _ ->
                                Files.Asc
                        )
                        (case sortedColumnS of
                            Just "date" ->
                                Files.Date

                            _ ->
                                Files.Name
                        )
                )
                (UrlParser.s "main" <?> intParam "error" <?> stringParam "sort" <?> stringParam "sortedColumn")
            ]


locationToPage : Maybe Api.User -> Navigation.Location -> ( Page, Cmd Msg )
locationToPage muser path =
    let
        location =
            path |> parsePath locParser

        login =
            case muser of
                Just user ->
                    ( NotFound, Navigation.newUrl "/main" )

                _ ->
                    let
                        ( login, cmd ) =
                            Login.init
                    in
                        ( Login login, Cmd.map LoginMsg cmd )

        settings =
            case muser of
                Just user ->
                    let
                        ( settings, cmd ) =
                            Settings.init user
                    in
                        ( Settings settings, Cmd.map SettingsMsg cmd )

                Nothing ->
                    ( NotFound, Navigation.newUrl "/index.html" )

        files i b sort sortedColumn =
            case muser of
                Just user ->
                    let
                        ( files, cmd ) =
                            Files.init user i b sort sortedColumn
                    in
                        ( Files files, Cmd.map FilesMsg cmd )

                Nothing ->
                    ( NotFound, Navigation.newUrl "/index.html" )

        logout =
            ( NotFound, Api.postLogout |> Http.send LogoutResult )
    in
        case location of
            Nothing ->
                login

            Just a ->
                case a of
                    LoginLoc ->
                        login

                    FilesLoc i b s sc ->
                        files i b s sc

                    SettingsLoc ->
                        settings

                    LogoutLoc ->
                        logout


init : Navigation.Location -> ( State, Cmd Msg )
init loc =
    BeforeLoggingInAttempt loc ! [ Api.postTestLogin |> Http.send LoginAttemptResult ]


urlChange : Model -> Navigation.Location -> ( Model, Cmd Msg )
urlChange model location =
    let
        user =
            case model.page of
                Login loginModel ->
                    loginModel.user

                Settings settingsModel ->
                    Just settingsModel.user

                _ ->
                    model.user

        ( page, pageCmd ) =
            locationToPage user location

        _ =
            Debug.log location.pathname page
    in
        { model | page = page, user = user } ! [ pageCmd ]


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        AfterLoggingInAttempt model ->
            case ( msg, model.page ) of
                ( LogoutResult _, p ) ->
                    AfterLoggingInAttempt { model | user = Nothing } ! [ Navigation.newUrl "index.html" ]

                ( UrlChange loc, p ) ->
                    let
                        ( model_, cmd ) =
                            urlChange model loc
                    in
                        AfterLoggingInAttempt model_ ! [ cmd ]

                ( LoginMsg msg, Login loginModel ) ->
                    let
                        ( login, cmd ) =
                            Login.update msg loginModel
                    in
                        AfterLoggingInAttempt { model | page = Login login } ! [ cmd |> Cmd.map LoginMsg ]

                ( SettingsMsg msg, Settings settingsModel ) ->
                    let
                        ( settings, cmd ) =
                            Settings.update msg settingsModel
                    in
                        AfterLoggingInAttempt { model | page = Settings settings } ! [ cmd |> Cmd.map SettingsMsg ]

                ( FilesMsg msg, Files filesModel ) ->
                    let
                        ( files, cmd ) =
                            Files.update msg filesModel
                    in
                        AfterLoggingInAttempt { model | page = Files files } ! [ cmd |> Cmd.map FilesMsg ]

                _ ->
                    state ! []

        BeforeLoggingInAttempt loc ->
            case msg of
                LoginAttemptResult res ->
                    let
                        muser =
                            (res |> Result.toMaybe |> Maybe.withDefault Nothing)

                        ( model_, cmd ) =
                            urlChange { user = muser, page = NotFound } loc
                    in
                        AfterLoggingInAttempt model_ ! [ cmd ]

                _ ->
                    state ! []


view : State -> Html Msg
view state =
    case state of
        AfterLoggingInAttempt model ->
            case model.page of
                Login login ->
                    Login.view login |> Html.map LoginMsg

                Files files ->
                    Files.view files |> Html.map FilesMsg

                Settings settings ->
                    Settings.view settings |> Html.map SettingsMsg

                NotFound ->
                    text ""

        _ ->
            text ""


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        AfterLoggingInAttempt model ->
            case model.page of
                Login login ->
                    Login.subscriptions login |> Sub.map LoginMsg

                Files files ->
                    Files.subscriptions files |> Sub.map FilesMsg

                Settings settings ->
                    Settings.subscriptions settings |> Sub.map SettingsMsg

                NotFound ->
                    Sub.none

        _ ->
            Sub.none
