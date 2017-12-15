module Files exposing (..)

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
    = Login
    | Register
    | DropdownMsg Dropdown.State
    | FileDropdown Int Dropdown.State
    | HelpersMsg Helpers.Msg
    | FileUploadModalMsg Modal.State
    | AddFolderModalMsg Modal.State
    | AddFolderSubmit
    | AddFolderNameSet String
    | AddFolderResult (Result Http.Error Api.FileStructure)


type alias Model =
    { dropdownState : Dropdown.State
    , fileDropdowns : List Dropdown.State
    , helpers : Helpers.Model
    , user : Api.User
    , fileUploadModalState : Modal.State
    , addFolderModalState : Modal.State
    , addFolderError : Maybe String
    , addFolderName : String
    }


init : Api.User -> ( Model, Cmd Msg )
init user =
    let
        ( helpers, cmd ) =
            Helpers.init
    in
        { helpers = helpers
        , dropdownState = Dropdown.initialState
        , fileDropdowns =
            List.repeat 4 Dropdown.initialState
        , user = user
        , fileUploadModalState = Modal.hiddenState
        , addFolderModalState = Modal.hiddenState
        , addFolderError = Nothing
        , addFolderName = ""
        }
            ! [ cmd |> Cmd.map HelpersMsg ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            model ! []

        Register ->
            model ! []

        DropdownMsg state ->
            { model | dropdownState = state } ! []

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

        HelpersMsg msg ->
            let
                ( helpers, cmd ) =
                    Helpers.update msg model.helpers
            in
                { model | helpers = helpers } ! [ cmd |> Cmd.map HelpersMsg ]

        FileUploadModalMsg state ->
            { model | fileUploadModalState = state } ! []

        AddFolderModalMsg state ->
            { model | addFolderModalState = state } ! []

        AddFolderSubmit ->
            case model.addFolderName of
                "" ->
                    { model | addFolderError = Just "Folder name cannot be empty" } ! []

                _ ->
                    { model | addFolderError = Nothing } ! [ Api.putApiFolderByFolderid 1 model.addFolderName |> Http.send AddFolderResult ]

        AddFolderNameSet s ->
            { model | addFolderError = Nothing, addFolderName = s } ! []

        AddFolderResult res ->
            let
                _ =
                    Debug.log "res" res
            in
                case res of
                    Ok _ ->
                        { model | addFolderModalState = Modal.hiddenState } ! []

                    _ ->
                        { model | addFolderError = Just "File with such name exists" } ! []


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


view : Model -> Html Msg
view model =
    Helpers.viewLoggedIn model.helpers HelpersMsg (viewContent model)


viewContent : Model -> Html Msg
viewContent model =
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
                        [ Dropdown.buttonItem [ class "pointer", onClick <| AddFolderModalMsg Modal.visibleState ] [ text "Add folder" ]
                        , Dropdown.buttonItem [ class "pointer", onClick <| FileUploadModalMsg Modal.visibleState ] [ text "Upload file" ]
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
        , Modal.config FileUploadModalMsg
            |> Modal.small
            |> Modal.h3 [] [ text "Upload file(s)" ]
            |> Modal.body []
                [ Html.form [ enctype "multipart/form-data", action <| "/fileUpload/1", method "POST" ]
                    [ input [ name "file", type_ "file" ] []
                    , p [] []
                    , Button.button
                        [ Button.outlineSuccess
                        , Button.attrs [ type_ "submit" ]
                        ]
                        [ text "Submit" ]
                    ]
                ]
            |> Modal.view model.fileUploadModalState
        , Modal.config AddFolderModalMsg
            |> Modal.small
            |> Modal.h3 [] [ text "Add folder" ]
            |> Modal.body []
                [ text "Folder name"
                , Input.text [ Input.attrs [ onInput AddFolderNameSet ] ]
                , text <| Maybe.withDefault "" model.addFolderError
                ]
            |> Modal.footer []
                [ Button.button [ Button.outlineSuccess, Button.attrs [ onClick AddFolderSubmit ] ] [ text "Submit" ] ]
            |> Modal.view model.addFolderModalState
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dropdownState DropdownMsg
        , model.fileDropdowns
            |> List.indexedMap (\i s -> Dropdown.subscriptions s <| FileDropdown i)
            |> Sub.batch
        , Helpers.subscriptions model.helpers |> Sub.map HelpersMsg
        ]
