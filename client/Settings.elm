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


type Msg
    = HelpersMsg Helpers.Msg
    | Submit


type alias Model =
    { helpers : Helpers.Model
    , user : Api.User
    }


init : Api.User -> ( Model, Cmd Msg )
init user =
    let
        ( helpers, cmd ) =
            Helpers.init
    in
        { helpers = helpers, user = user } ! [ cmd |> Cmd.map HelpersMsg ]


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
            model ! []


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
                    , Button.button [ Button.primary, Button.onClick Submit ] [ text "Submit" ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
