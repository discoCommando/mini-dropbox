module Login exposing (..)

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
import Api
import Http


type Msg
    = Login
    | Register
    | TabMsg Tab.State
    | LoginResult (Result Http.Error (Maybe Api.User))
    | LoginUsernameSet String
    | LoginPasswordSet String
    | RegisterResult (Result Http.Error (Maybe Api.User))
    | RegisterUsernameSet String
    | RegisterPasswordSet String
    | RegisterConfirmPasswordSet String


type alias Model =
    { tabState : Tab.State
    , loginUsername : String
    , loginPassword : String
    , loginError : Maybe String
    , registerUsername : String
    , registerPassword : String
    , registerConfirmPassword : String
    , registerError : Maybe String
    , user : Maybe Api.User
    }


init : ( Model, Cmd Msg )
init =
    { tabState = Tab.initialState
    , loginUsername = ""
    , loginPassword = ""
    , loginError = Nothing
    , registerUsername = ""
    , registerPassword = ""
    , registerConfirmPassword = ""
    , registerError = Nothing
    , user = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            { model | loginError = Nothing } ! [ Api.postLogin { username = model.loginUsername, password = model.loginPassword } |> Http.send LoginResult ]

        Register ->
            case model.registerPassword == model.registerConfirmPassword of
                True ->
                    { model | registerError = Nothing } ! [ Api.postRegister { username = model.registerUsername, password = model.registerPassword } |> Http.send RegisterResult ]

                False ->
                    { model | registerError = Just "Password are not the same" } ! []

        TabMsg state ->
            { model | tabState = state } ! []

        LoginResult res ->
            case res of
                Ok (Just user) ->
                    { model | user = Just user } ! [ Navigation.newUrl "main" ]

                _ ->
                    { model | loginError = Just "Wrong username or password" } ! []

        LoginUsernameSet s ->
            { model | loginUsername = s, loginError = Nothing } ! []

        LoginPasswordSet s ->
            { model | loginPassword = s } ! []

        RegisterResult res ->
            case res of
                Ok (Just user) ->
                    { model | user = Just user } ! [ Navigation.newUrl "main" ]

                _ ->
                    { model | registerError = Just "Username already exists" } ! []

        RegisterUsernameSet s ->
            { model | registerUsername = s, registerError = Nothing } ! []

        RegisterPasswordSet s ->
            { model | registerPassword = s } ! []

        RegisterConfirmPasswordSet s ->
            { model | registerConfirmPassword = s } ! []


view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ class "middle" ] ]
            [ Grid.col
                [ Col.lg4
                , Col.md3
                , Col.sm2
                , Col.xs1
                , Col.attrs [ class "custom-class" ]
                  -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
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
                                                        [ Grid.col [] <|
                                                            List.concat
                                                                [ [ Form.group []
                                                                        [ InputGroup.config
                                                                            (InputGroup.text
                                                                                [ Input.attrs [ placeholder "Username", onInput LoginUsernameSet, value model.loginUsername ]
                                                                                ]
                                                                            )
                                                                            |> InputGroup.successors
                                                                                [ InputGroup.span [] [ i [ class "fa fa-user", attribute "aria-hidden" "true" ] [] ] ]
                                                                            |> InputGroup.view
                                                                        ]
                                                                  , Form.group []
                                                                        [ InputGroup.config
                                                                            (InputGroup.password [ Input.attrs [ placeholder "Password", onInput LoginPasswordSet, value model.loginPassword ] ])
                                                                            |> InputGroup.successors
                                                                                [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                            |> InputGroup.view
                                                                        ]
                                                                  , Button.button
                                                                        [ Button.primary
                                                                        , Button.block
                                                                        , Button.attrs [ onClick Login ]
                                                                        ]
                                                                        [ text "Sign in" ]
                                                                  ]
                                                                , case model.loginError of
                                                                    Just error ->
                                                                        [ text error ]

                                                                    _ ->
                                                                        []
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
                                                        [ Grid.col [] <|
                                                            List.concat
                                                                [ [ Form.group []
                                                                        [ InputGroup.config
                                                                            (InputGroup.text
                                                                                [ Input.attrs [ placeholder "Username", onInput RegisterUsernameSet, value model.registerUsername ]
                                                                                ]
                                                                            )
                                                                            |> InputGroup.successors
                                                                                [ InputGroup.span [] [ i [ class "fa fa-user", attribute "aria-hidden" "true" ] [] ] ]
                                                                            |> InputGroup.view
                                                                        ]
                                                                  , Form.group []
                                                                        [ InputGroup.config
                                                                            (InputGroup.password [ Input.attrs [ placeholder "Password", onInput RegisterPasswordSet, value model.registerPassword ] ])
                                                                            |> InputGroup.successors
                                                                                [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                            |> InputGroup.view
                                                                        ]
                                                                  , Form.group []
                                                                        [ InputGroup.config
                                                                            (InputGroup.password [ Input.attrs [ placeholder "Confirm Password", onInput RegisterConfirmPasswordSet, value model.registerConfirmPassword ] ])
                                                                            |> InputGroup.successors
                                                                                [ InputGroup.span [] [ i [ class "fa fa-lock", attribute "aria-hidden" "true" ] [] ] ]
                                                                            |> InputGroup.view
                                                                        ]
                                                                  , Button.button
                                                                        [ Button.success
                                                                        , Button.block
                                                                        , Button.attrs [ onClick Register ]
                                                                        ]
                                                                        [ text "Sign up" ]
                                                                  ]
                                                                , case model.registerError of
                                                                    Just error ->
                                                                        [ text error ]

                                                                    _ ->
                                                                        []
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
                , Col.attrs [ class "custom-class" ]
                  -- <module>.attrs function, creates an option to specify a list of custom Elm Html attributes.
                ]
                []
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Tab.subscriptions model.tabState TabMsg
        ]
