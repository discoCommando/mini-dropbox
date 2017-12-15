module Helpers exposing (..)

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
import Http
import Api


type Msg
    = NavbarMsg Navbar.State
    | GotoFiles
    | GotoSettings
    | Logout


type alias Model =
    { navbarState : Navbar.State }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        { navbarState = navbarState } ! [ navbarCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            { model | navbarState = state } ! []

        GotoFiles ->
            model ! [ Navigation.newUrl "/main" ]

        GotoSettings ->
            model ! [ Navigation.newUrl "/settings" ]

        Logout ->
            model ! [ Navigation.newUrl "/logout" ]


viewLoggedIn : Model -> Api.User -> (Msg -> msg) -> Html msg -> Html msg
viewLoggedIn model user lift innerView =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ class "middle" ] ]
            [ Grid.col
                [ Col.md1
                , Col.attrs [ class "custom-class" ]
                  -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            , Grid.col
                [ Col.md10 ]
                [ Card.config [ Card.align Text.alignXsCenter ]
                    |> Card.headerH4 []
                        [ Navbar.config (lift << NavbarMsg)
                            |> Navbar.attrs [ class "justify-content-end" ]
                            |> Navbar.withAnimation
                            |> Navbar.brand [ onClick <| lift GotoFiles, class "pointer" ] [ i [ class "fa fa-folder-o fa-3x", attribute "aria-hidden" "true" ] [] ]
                            |> Navbar.items []
                            |> Navbar.customItems
                                -- Add custom items
                                [ Navbar.textItem [ class "center navbar-right-item " ]
                                    [ b [] [ text user.userLogin ]
                                    ]
                                , Navbar.textItem [ class "center navbar-right-item pointer", onClick <| lift GotoSettings ]
                                    [ i [ class "fa fa-user-circle navbar-right-item-icon", attribute "aria-hidden" "true" ] []
                                    , text "Settings"
                                    ]
                                , Navbar.textItem [ class "center pointer", onClick <| lift Logout ]
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
                , Col.attrs [ class "custom-class" ]
                  -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        ]
