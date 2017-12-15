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
import Date exposing (Date)
import Date.Extra


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
    | FileStructureLoadResult (Result Http.Error Api.FileStructure)


type File
    = Folder Api.Folder
    | File Api.File


type alias Model =
    { parentId : Int
    , files : List ( File, Dropdown.State )
    , dropdownState : Dropdown.State
    , helpers : Helpers.Model
    , user : Api.User
    , fileUploadModalState : Modal.State
    , addFolderModalState : Modal.State
    , addFolderError : Maybe String
    , addFolderName : String
    }


init : Api.User -> Maybe Int -> ( Model, Cmd Msg )
init user mparentId =
    let
        ( helpers, cmd ) =
            Helpers.init
    in
        { parentId = Maybe.withDefault 0 mparentId
        , files = []
        , helpers = helpers
        , dropdownState = Dropdown.initialState
        , user = user
        , fileUploadModalState = Modal.hiddenState
        , addFolderModalState = Modal.hiddenState
        , addFolderError = Nothing
        , addFolderName = ""
        }
            ! [ cmd |> Cmd.map HelpersMsg, Api.getApiFolderByFolderid (Maybe.withDefault 0 mparentId) |> Http.send FileStructureLoadResult ]


fileStructureToFiles : Api.FileStructure -> List ( File, Dropdown.State )
fileStructureToFiles fileStructure =
    List.map File fileStructure.files
        ++ List.map Folder fileStructure.folders
        |> List.map (\x -> ( x, Dropdown.initialState ))


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

                        ( x, s ) :: xs ->
                            if id <= 0 then
                                ( x, state ) :: xs
                            else
                                ( x, s ) :: replace (id - 1) xs
            in
                { model | files = replace i model.files } ! []

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
                    { model | addFolderError = Nothing } ! [ Api.putApiFolderByFolderid model.parentId model.addFolderName |> Http.send AddFolderResult ]

        AddFolderNameSet s ->
            { model | addFolderError = Nothing, addFolderName = s } ! []

        AddFolderResult res ->
            let
                _ =
                    Debug.log "res" res
            in
                case res of
                    Ok files ->
                        { model | addFolderModalState = Modal.hiddenState, files = fileStructureToFiles files } ! []

                    _ ->
                        { model | addFolderError = Just "File with such name exists" } ! []

        FileStructureLoadResult res ->
            case res of
                Ok fs ->
                    { model | files = fileStructureToFiles fs } ! []

                _ ->
                    model ! []


view : Model -> Html Msg
view model =
    Helpers.viewLoggedIn model.helpers HelpersMsg (viewContent model)


getFileInsertDate : File -> Date
getFileInsertDate file =
    case file of
        File file ->
            file.fileInsertDate

        Folder folder ->
            folder.folderInsertDate


getIconText : File -> String
getIconText file =
    case file of
        File file ->
            --TODO change
            "fa-folder"

        Folder folder ->
            "fa-folder"


getName : File -> String
getName file =
    case file of
        File file ->
            file.fileName

        Folder folder ->
            folder.folderName


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
                        Table.tbody [] <|
                            List.indexedMap
                                (\id ( file, dropdownState ) ->
                                    Table.tr []
                                        [ Table.td []
                                            [ i [ class <| "fa " ++ getIconText file ++ " filetype", attribute "aria-hidden" "true" ] []
                                            , text <| getName file
                                            ]
                                        , Table.td [] [ text <| Date.Extra.toFormattedString "dd-MMM-YYYY hh:mm:ss" <| getFileInsertDate file ]
                                        , Table.td []
                                            [ Dropdown.dropdown
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
                                            ]
                                        ]
                                )
                                model.files
                        --[ Table.tr []
                        --    [ Table.td []
                        --        [ i [ class "fa fa-folder filetype", attribute "aria-hidden" "true" ] []
                        --        , text "Folder"
                        --        ]
                        --    , Table.td [] [ text "20-Oct-2017 15:52:51" ]
                        --    , Table.td [] [ fileActions 0 model ]
                        --    ]
                        --, Table.tr []
                        --    [ Table.td []
                        --        [ i [ class "fa fa-file-pdf-o filetype", attribute "aria-hidden" "true" ] []
                        --        , text "file.pdf"
                        --        ]
                        --    , Table.td [] [ text "20-Oct-2017 15:52:52" ]
                        --    , Table.td [] [ fileActions 1 model ]
                        --    ]
                        --, Table.tr []
                        --    [ Table.td []
                        --        [ i [ class "fa fa-file-o filetype", attribute "aria-hidden" "true" ] []
                        --        , text "notes.txt"
                        --        ]
                        --    , Table.td [] [ text "15-Oct-2017 14:30:10" ]
                        --    , Table.td [] [ fileActions 2 model ]
                        --    ]
                        --, Table.tr []
                        --    [ Table.td []
                        --        [ i [ class "fa fa-file-image-o filetype", attribute "aria-hidden" "true" ] []
                        --        , text "image.png"
                        --        ]
                        --    , Table.td [] [ text "15-Oct-2017 14:30:10" ]
                        --    , Table.td [] [ fileActions 3 model ]
                        --    ]
                        --]
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
        , model.files
            |> List.indexedMap (\i ( _, s ) -> Dropdown.subscriptions s <| FileDropdown i)
            |> Sub.batch
        , Helpers.subscriptions model.helpers |> Sub.map HelpersMsg
        ]
