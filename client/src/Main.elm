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


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type Msg
    = UrlChange Navigation.Location
    | ModalMsg Modal.State
    | NavbarMsg Navbar.State
    | DropdownMsg Dropdown.State
    | FileDropdown Int Dropdown.State
    | TabMsg Tab.State
    | GotoMain
    | GotoLogin
    | GotoSettings


type Page
    = Login
    | Main
    | Settings
    | NotFound


type alias Model =
    { modalState : Modal.State
    , navbarState : Navbar.State
    , dropdownState : Dropdown.State
    , tabState : Tab.State
    , fileDropdowns : List Dropdown.State
    , page : Page
    }


locationToPage : Navigation.Location -> Page
locationToPage location =
    let
        suffix =
            location.pathname
                |> String.split "/"
                |> List.reverse
                |> List.head
    in
        case suffix of
            Nothing ->
                Login

            Just a ->
                case a of
                    "index.html" ->
                        Login

                    "main" ->
                        Main

                    "main?" ->
                        Main

                    "settings" ->
                        Settings

                    "" ->
                        Login

                    _ ->
                        NotFound


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        { modalState = Modal.visibleState
        , page = locationToPage location
        , navbarState = navbarState
        , dropdownState = Dropdown.initialState
        , tabState = Tab.initialState
        , fileDropdowns =
            List.repeat 4 Dropdown.initialState
        }
            ! [ navbarCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange loc ->
            { model | page = locationToPage loc } ! []

        ModalMsg modalState ->
            { model | modalState = modalState } ! []

        NavbarMsg state ->
            { model | navbarState = state } ! []

        DropdownMsg state ->
            { model | dropdownState = state } ! []

        TabMsg state ->
            { model | tabState = state } ! []

        FileDropdown i state ->
            let
                replace id list =
                    case list of
                        [] ->
                            []

                        x :: xs ->
                            if id <= 0 then
                                state :: xs
                            else
                                x :: replace (id - 1) xs
            in
                { model | fileDropdowns = replace i model.fileDropdowns } ! []

        GotoMain ->
            model ! [ Navigation.newUrl "main" ]

        GotoLogin ->
            model ! [ Navigation.newUrl "index.html" ]

        GotoSettings ->
            model ! [ Navigation.newUrl "settings" ]


view : Model -> Html Msg
view model =
    case model.page of
        Login ->
            viewLogin model

        Main ->
            viewLoggedIn model <| viewMain model

        Settings ->
            viewLoggedIn model <| viewSettings model

        NotFound ->
            text "NotFound"


viewLogin : Model -> Html Msg
viewLogin model =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ class "middle" ] ]
            [ Grid.col
                [ Col.lg4
                , Col.md3
                , Col.sm2
                , Col.xs1
                , Col.attrs [ class "custom-class" ] -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            , Grid.col
                [ Col.lg4
                , Col.md6
                , Col.sm8
                , Col.xs10
                ]
                [ Card.config [ Card.align Text.alignXsCenter ]
                    |> Card.block [ Card.blockAttrs [ class "login-form" ] ]
                        [ Card.custom
                            (Tab.config TabMsg
                                |> Tab.withAnimation
                                |> Tab.items
                                    [ Tab.item
                                        { id = "tabItem1"
                                        , link = Tab.link [ class "login-header" ] [ h5 [] [ text "Login" ] ]
                                        , pane =
                                            Tab.pane [ class "mt-3 login-tab-content" ]
                                                [ Grid.container []
                                                    [ Grid.row
                                                        []
                                                        [ Grid.col []
                                                            [ Form.group []
                                                                [ InputGroup.config
                                                                    (InputGroup.text
                                                                        [ Input.attrs [ placeholder "Username" ]
                                                                        ]
                                                                    )
                                                                    |> InputGroup.successors
                                                                        [ InputGroup.span [] [ i [ class "fa fa-user", attribute "aria-hidden" "true" ] [] ] ]
                                                                    |> InputGroup.view
                                                                ]
                                                            , Form.group []
                                                                [ InputGroup.config
                                                                    (InputGroup.password [ Input.attrs [ placeholder "Password" ] ])
                                                                    |> InputGroup.successors
                                                                        [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                    |> InputGroup.view
                                                                ]
                                                            , Button.button
                                                                [ Button.primary
                                                                , Button.block
                                                                , Button.attrs [ onClick GotoMain ]
                                                                ]
                                                                [ text "Sign in" ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                        }
                                    , Tab.item
                                        { id = "tabItem2"
                                        , link = Tab.link [ class "register-header" ] [ h5 [] [ text "Register" ] ]
                                        , pane =
                                            Tab.pane [ class "mt-3 login-tab-content" ]
                                                [ Grid.container []
                                                    [ Grid.row
                                                        []
                                                        [ Grid.col []
                                                            [ Form.group []
                                                                [ InputGroup.config
                                                                    (InputGroup.text
                                                                        [ Input.attrs [ placeholder "Username" ]
                                                                        ]
                                                                    )
                                                                    |> InputGroup.successors
                                                                        [ InputGroup.span [] [ i [ class "fa fa-user", attribute "aria-hidden" "true" ] [] ] ]
                                                                    |> InputGroup.view
                                                                ]
                                                            , Form.group []
                                                                [ InputGroup.config
                                                                    (InputGroup.password [ Input.attrs [ placeholder "Password" ] ])
                                                                    |> InputGroup.successors
                                                                        [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                    |> InputGroup.view
                                                                ]
                                                            , Form.group []
                                                                [ InputGroup.config
                                                                    (InputGroup.password [ Input.attrs [ placeholder "Confirm Password" ] ])
                                                                    |> InputGroup.successors
                                                                        [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                    |> InputGroup.view
                                                                ]
                                                            , Button.button
                                                                [ Button.success
                                                                , Button.block
                                                                , Button.attrs [ onClick GotoMain ]
                                                                ]
                                                                [ text "Sign up" ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                        }
                                    ]
                                |> Tab.justified
                                |> Tab.view model.tabState
                            )
                        ]
                    |> Card.view
                ]
            , Grid.col
                [ Col.lg4
                , Col.md3
                , Col.sm2
                , Col.xs1
                , Col.attrs [ class "custom-class" ] -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            ]
        ]


fileActions : Int -> Model -> Html Msg
fileActions id model =
    let
        dropdownState =
            List.drop id model.fileDropdowns
                |> List.head
                |> Maybe.withDefault model.dropdownState
    in
        Dropdown.dropdown
            dropdownState
            { options = []
            , toggleMsg = FileDropdown id
            , toggleButton =
                Dropdown.toggle
                    [ Button.secondary, Button.attrs [ class "file-action" ], Button.small ]
                    [ i [ class "fa fa-ellipsis-h", attribute "aria-hidden" "true" ] [] ]
            , items =
                [ Dropdown.buttonItem [ class "pointer" ] [ text "Rename" ]
                , Dropdown.buttonItem [ class "pointer" ] [ text "Delete" ]
                , Dropdown.buttonItem [ class "pointer" ] [ text "Cut" ]
                , Dropdown.buttonItem [ class "pointer" ] [ text "Copy" ]
                ]
            }


viewLoggedIn : Model -> Html Msg -> Html Msg
viewLoggedIn model innerView =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ class "middle" ] ]
            [ Grid.col
                [ Col.md1
                , Col.attrs [ class "custom-class" ] -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            , Grid.col
                [ Col.md10 ]
                [ Card.config [ Card.align Text.alignXsCenter ]
                    |> Card.headerH4 []
                        [ Navbar.config NavbarMsg
                            |> Navbar.attrs [ class "justify-content-end" ]
                            |> Navbar.withAnimation
                            |> Navbar.brand [ onClick GotoMain, class "pointer" ] [ i [ class "fa fa-folder-o fa-3x", attribute "aria-hidden" "true" ] [] ]
                            |> Navbar.items []
                            |> Navbar.customItems
                                -- Add custom items
                                [ Navbar.formItem [ class "center navbar-right-item pointer" ]
                                    [ InputGroup.config
                                        (InputGroup.text
                                            [ Input.attrs [ placeholder "Search" ]
                                            ]
                                        )
                                        |> InputGroup.successors
                                            [ InputGroup.button [ Button.onClick GotoMain ] [ i [ class "fa fa-search", attribute "aria-hidden" "true" ] [] ] ]
                                        |> InputGroup.view
                                    ]
                                , Navbar.textItem [ class "center navbar-right-item pointer", onClick GotoSettings ]
                                    [ i [ class "fa fa-user-circle navbar-right-item-icon", attribute "aria-hidden" "true" ] []
                                    , text "Settings"
                                    ]
                                , Navbar.textItem [ class "center pointer", onClick GotoLogin ]
                                    [ i [ class "fa fa-sign-out navbar-right-item-icon", attribute "aria-hidden" "true" ] []
                                    , text "Sign out"
                                    ]
                                ]
                            |> Navbar.view model.navbarState
                        ]
                    |> Card.block []
                        [ Card.custom innerView
                        ]
                    |> Card.footer []
                        [ Grid.container []
                            [ Grid.row []
                                [ Grid.col
                                    [ Col.attrs [ class "text-left" ] ]
                                    [ text "Used: 902MB/2048MB" ]
                                , Grid.col
                                    [ Col.attrs [ class "text-right" ] ]
                                    [ text "Tomasz Wawreniuk"
                                    , i [ class "fa fa-copyright", attribute "aria-hidden" "true" ] []
                                    ]
                                ]
                            ]
                        ]
                    |> Card.view
                ]
            , Grid.col
                [ Col.md1
                , Col.attrs [ class "custom-class" ] -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    Grid.container []
        [ Grid.row [ Row.attrs [ class "folders" ] ]
            [ Grid.col
                [ Col.xs8, Col.attrs [ class "text-left" ] ]
                [ h4 []
                    [ span [] [ text "Folder1" ]
                    , span [] [ i [ class "fa fa-caret-right", attribute "aria-hidden" "true" ] [] ]
                    , span [] [ text "Folder2" ]
                    ]
                ]
            , Grid.col
                [ Col.xs4, Col.attrs [ class "text-right" ] ]
                [ Dropdown.dropdown
                    model.dropdownState
                    { options = []
                    , toggleMsg = DropdownMsg
                    , toggleButton =
                        Dropdown.toggle
                            [ Button.secondary ]
                            [ text "Actions" ]
                    , items =
                        [ Dropdown.buttonItem [ class "pointer" ] [ text "Add folder" ]
                        , Dropdown.buttonItem [ class "pointer" ] [ text "Upload file" ]
                        , Dropdown.buttonItem [ class "pointer" ] [ text "Paste" ]
                        ]
                    }
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ Table.table
                    { options = [ Table.hover ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [ Table.cellAttr (class "w-75") ] [ text "Name" ]
                            , Table.th [ Table.cellAttr (class "w-25") ] [ text "Last modified" ]
                            , Table.th [ Table.cellAttr (class "w-10") ] []
                            ]
                    , tbody =
                        Table.tbody []
                            [ Table.tr []
                                [ Table.td []
                                    [ i [ class "fa fa-folder filetype", attribute "aria-hidden" "true" ] []
                                    , text "Folder"
                                    ]
                                , Table.td [] [ text "20-Oct-2017 15:52:51" ]
                                , Table.td [] [ fileActions 0 model ]
                                ]
                            , Table.tr []
                                [ Table.td []
                                    [ i [ class "fa fa-file-pdf-o filetype", attribute "aria-hidden" "true" ] []
                                    , text "file.pdf"
                                    ]
                                , Table.td [] [ text "20-Oct-2017 15:52:52" ]
                                , Table.td [] [ fileActions 1 model ]
                                ]
                            , Table.tr []
                                [ Table.td []
                                    [ i [ class "fa fa-file-o filetype", attribute "aria-hidden" "true" ] []
                                    , text "notes.txt"
                                    ]
                                , Table.td [] [ text "15-Oct-2017 14:30:10" ]
                                , Table.td [] [ fileActions 2 model ]
                                ]
                            , Table.tr []
                                [ Table.td []
                                    [ i [ class "fa fa-file-image-o filetype", attribute "aria-hidden" "true" ] []
                                    , text "image.png"
                                    ]
                                , Table.td [] [ text "15-Oct-2017 14:30:10" ]
                                , Table.td [] [ fileActions 3 model ]
                                ]
                            ]
                    }
                ]
            ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.attrs [ class "text-left" ] ]
                [ h2 [] [ text "Settings" ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md6, Col.attrs [ class "text-left" ] ]
                [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "Login" ]
                        , Input.text []
                        ]
                    , Form.group []
                        [ Form.label [] [ text "Password" ]
                        , Input.password []
                        , Form.help [] [ text "Make sure your password is safe!" ]
                        ]
                    , Form.group []
                        [ Form.label [ for "myselect" ] [ text "Theme" ]
                        , Select.select [ Select.id "myselect" ]
                            [ Select.item [] [ text "Light" ]
                            , Select.item [] [ text "Dark" ]
                            ]
                        ]
                    , Button.button [ Button.primary, Button.onClick GotoMain ] [ text "Submit" ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Dropdown.subscriptions model.dropdownState DropdownMsg
        , Tab.subscriptions model.tabState TabMsg
        , model.fileDropdowns
            |> List.indexedMap (\i s -> Dropdown.subscriptions s <| FileDropdown i)
            |> Sub.batch
        ]
