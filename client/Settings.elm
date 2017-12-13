module Settings exposing (..)

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
import Helpers
import Api
import Http


type Msg
    = HelpersMsg Helpers.Msg
    | UsernameSet String
    | PasswordSet String
    | PreferenceSet Int
    | Submit
    | SubmitResult (Result Http.Error (Maybe Api.User))


type alias Model =
    { helpers : Helpers.Model
    , user : Api.User
    , username : String
    , password : String
    , preference : Int
    , error : Maybe String
    }


init : Api.User -> ( Model, Cmd Msg )
init user =
    let
        ( helpers, cmd ) =
            Helpers.init
    in
        { helpers = helpers
        , user = user
        , username = user.userLogin
        , password = ""
        , preference = user.userPreference
        , error = Nothing
        }
            ! [ cmd |> Cmd.map HelpersMsg ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HelpersMsg msg ->
            let
                ( helpers, cmd ) =
                    Helpers.update msg model.helpers
            in
                { model | helpers = helpers } ! [ cmd |> Cmd.map HelpersMsg ]

        Submit ->
            let
                user =
                    model.user
            in
                { model | error = Nothing }
                    ! [ Api.postApiUser
                            { user
                                | userLogin = model.username
                                , userPassword = model.password
                                , userPreference = model.preference
                            }
                            |> Http.send SubmitResult
                      ]

        SubmitResult res ->
            case res of
                Ok (Just user) ->
                    { model | user = user } ! [ Navigation.newUrl "main" ]

                _ ->
                    { model | error = Just "Username already exists" } ! []

        UsernameSet s ->
            { model | username = s, error = Nothing } ! []

        PasswordSet s ->
            { model | password = s } ! []

        PreferenceSet i ->
            { model | preference = i } ! []


view : Model -> Html Msg
view model =
    Helpers.viewLoggedIn model.helpers HelpersMsg (viewContent model)


viewContent : Model -> Html Msg
viewContent model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.attrs [ class "text-left" ] ]
                [ h2 [] [ text "Settings" ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md6, Col.attrs [ class "text-left" ] ]
                [ Form.form [] <|
                    List.concat
                        [ case model.error of
                            Just error ->
                                [ text error ]

                            _ ->
                                []
                        , [ Form.group []
                                [ Form.label [] [ text "Login" ]
                                , Input.text [ Input.attrs [ onInput UsernameSet, value model.username ] ]
                                ]
                          , Form.group []
                                [ Form.label [] [ text "Password" ]
                                , Input.password [ Input.attrs [ onInput PasswordSet, value model.password ] ]
                                , Form.help [] [ text "Make sure your password is safe!" ]
                                ]
                          , Form.group []
                                [ Form.label [ for "myselect" ] [ text "Theme" ]
                                , Select.select [ Select.id "myselect" ]
                                    [ Select.item [ onClick <| PreferenceSet 0, selected <| model.preference == 0 ] [ text "Light" ]
                                    , Select.item [ onClick <| PreferenceSet 1, selected <| model.preference == 1 ] [ text "Dark" ]
                                    ]
                                ]
                          , Button.button [ Button.primary, Button.onClick Submit ] [ text "Submit" ]
                          ]
                        ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
