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


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }



--type BootstrapMsg
--    = UrlChange Navigation.Location
--    | ModalMsg Modal.State
--    | NavbarMsg Navbar.State
--    | DropdownMsg Dropdown.State
--    | FileDropdown Int Dropdown.State
--    | TabMsg Tab.State
--    | GotoMain
--    | GotoLogin
--    | GotoSettings


type Msg
    = LoginMsg Login.Msg
    | FilesMsg Files.Msg
    | SettingsMsg Settings.Msg
    | UrlChange Navigation.Location


type Page
    = Login Login.Model
    | Files Files.Model
    | Settings Settings.Model
    | NotFound


type alias Model =
    { page : Page
    }


locationToPage : Navigation.Location -> ( Page, Cmd Msg )
locationToPage location =
    let
        suffix =
            location.pathname
                |> String.split "/"
                |> List.reverse
                |> List.head

        login =
            let
                ( login, cmd ) =
                    Login.init
            in
                ( Login login, Cmd.map LoginMsg cmd )

        settings =
            let
                ( settings, cmd ) =
                    Settings.init
            in
                ( Settings settings, Cmd.map SettingsMsg cmd )

        files =
            let
                ( files, cmd ) =
                    Files.init
            in
                ( Files files, Cmd.map FilesMsg cmd )
    in
        case suffix of
            Nothing ->
                login

            Just a ->
                case a of
                    "index.html" ->
                        login

                    "main" ->
                        files

                    "main?" ->
                        files

                    "settings" ->
                        settings

                    "" ->
                        login

                    _ ->
                        NotFound ! []


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( page, pageCmd ) =
            locationToPage location
    in
        { page = page
        }
            ! [ pageCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChange loc, p ) ->
            let
                ( page, cmd ) =
                    locationToPage loc
            in
                { model | page = page } ! [ cmd ]

        ( LoginMsg msg, Login loginModel ) ->
            let
                ( login, cmd ) =
                    Login.update msg loginModel
            in
                { model | page = Login login } ! [ cmd |> Cmd.map LoginMsg ]

        ( SettingsMsg msg, Settings settingsModel ) ->
            let
                ( settings, cmd ) =
                    Settings.update msg settingsModel
            in
                { model | page = Settings settings } ! [ cmd |> Cmd.map SettingsMsg ]

        ( FilesMsg msg, Files filesModel ) ->
            let
                ( files, cmd ) =
                    Files.update msg filesModel
            in
                { model | page = Files files } ! [ cmd |> Cmd.map FilesMsg ]

        _ ->
            model ! []



--ModalMsg modalState ->
--    { model | modalState = modalState } ! []
--GotoMain ->
--    model ! [ Navigation.newUrl "main" ]
--GotoLogin ->
--    model ! [ Navigation.newUrl "index.html" ]
--GotoSettings ->
--    model ! [ Navigation.newUrl "settings" ]


view : Model -> Html Msg
view model =
    case model.page of
        Login login ->
            Login.view login |> Html.map LoginMsg

        Files files ->
            Files.view files |> Html.map FilesMsg

        Settings settings ->
            Settings.view settings |> Html.map SettingsMsg

        NotFound ->
            text "NotFound"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Login login ->
            Login.subscriptions login |> Sub.map LoginMsg

        Files files ->
            Files.subscriptions files |> Sub.map FilesMsg

        Settings settings ->
            Settings.subscriptions settings |> Sub.map SettingsMsg

        NotFound ->
            Sub.none
