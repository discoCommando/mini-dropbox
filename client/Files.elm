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
    | RenameFileModalMsg Modal.State
    | RenameFileSubmit
    | RenameFileNameSet String
    | RenameFileResult (Result Http.Error Api.FileStructure)
    | RenameFileInit File
    | DeleteFile File
    | DeleteFileResult (Result Http.Error Api.FileStructure)
    | Open File
    | FolderChainLoadResult (Result Http.Error (List Api.Folder))
    | ChangeSort SortedColumn


type File
    = Folder Api.Folder
    | File Api.File


type Sort
    = Desc
    | Asc


sortNext : Sort -> Sort
sortNext sort =
    case sort of
        Desc ->
            Asc

        Asc ->
            Desc


sortToString : Sort -> String
sortToString sort =
    case sort of
        Desc ->
            "desc"

        Asc ->
            "asc"


sortedColumnToString : SortedColumn -> String
sortedColumnToString sortedColumn =
    case sortedColumn of
        Date ->
            "date"

        Name ->
            "name"


type SortedColumn
    = Name
    | Date


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
    , renameFileModalState : Modal.State
    , renameFileError : Maybe String
    , renameFileName : String
    , renameFileCurrent : Maybe File
    , folderChain : List Api.Folder
    , capacity : Int
    , errorMsg : Maybe String
    , sort : Sort
    , sortedColumn : SortedColumn
    }


init : Api.User -> Maybe Int -> Bool -> Sort -> SortedColumn -> ( Model, Cmd Msg )
init user mparentId isError sort sortedColumn =
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
        , renameFileModalState = Modal.hiddenState
        , renameFileError = Nothing
        , renameFileName = ""
        , renameFileCurrent = Nothing
        , folderChain = []
        , capacity = 0
        , errorMsg =
            if isError then
                Just "Size limit exceeded"
            else
                Nothing
        , sort = sort
        , sortedColumn = sortedColumn
        }
            ! [ cmd |> Cmd.map HelpersMsg
              , Api.getApiFolderByFolderid (Maybe.withDefault 0 mparentId) |> Http.send FileStructureLoadResult
              , Api.getApiFolderChainByFolderid (Maybe.withDefault 0 mparentId) |> Http.send FolderChainLoadResult
              ]


fileStructureToFiles : Api.FileStructure -> Sort -> SortedColumn -> List ( File, Dropdown.State )
fileStructureToFiles fileStructure sort sortedColumn =
    (List.map File fileStructure.files
        ++ List.map Folder fileStructure.folders
    )
        |> (case sortedColumn of
                Date ->
                    List.sortBy (getFileInsertDate >> toString)

                Name ->
                    List.sortBy (getName >> String.toLower)
           )
        |> (case sort of
                Desc ->
                    List.reverse

                _ ->
                    identity
           )
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
            case res of
                Ok files ->
                    { model | addFolderModalState = Modal.hiddenState, files = fileStructureToFiles files model.sort model.sortedColumn } ! []

                _ ->
                    { model | addFolderError = Just "File with such name exists" } ! []

        FileStructureLoadResult res ->
            case res of
                Ok fs ->
                    { model | files = fileStructureToFiles fs model.sort model.sortedColumn } ! []

                _ ->
                    model ! []

        RenameFileInit file ->
            { model | renameFileModalState = Modal.visibleState, renameFileCurrent = Just file, renameFileName = getName file } ! []

        RenameFileModalMsg state ->
            { model | renameFileModalState = state } ! []

        RenameFileSubmit ->
            case model.renameFileCurrent of
                Just file ->
                    case model.renameFileName of
                        "" ->
                            { model | renameFileError = Just "Folder name cannot be empty" } ! []

                        _ ->
                            let
                                send x =
                                    case model.renameFileName == getName file of
                                        True ->
                                            Cmd.none

                                        False ->
                                            Http.send RenameFileResult x
                            in
                                case file of
                                    File f ->
                                        { model | renameFileError = Nothing } ! [ Api.postApiFileRenameByFileid f.fileId model.renameFileName |> send ]

                                    Folder f ->
                                        { model | renameFileError = Nothing } ! [ Api.postApiFolderRenameByFolderid f.folderId model.renameFileName |> send ]

                Nothing ->
                    model ! []

        RenameFileNameSet s ->
            { model | renameFileError = Nothing, renameFileName = s } ! []

        RenameFileResult res ->
            case res of
                Ok files ->
                    { model | renameFileModalState = Modal.hiddenState, files = fileStructureToFiles files model.sort model.sortedColumn } ! []

                _ ->
                    { model | renameFileError = Just "File with such name exists" } ! []

        DeleteFile file ->
            case file of
                File file ->
                    model ! [ Api.deleteApiFileByFileid file.fileId |> Http.send DeleteFileResult ]

                Folder folder ->
                    model ! [ Api.deleteApiFolderByFolderid folder.folderId |> Http.send DeleteFileResult ]

        DeleteFileResult res ->
            case res of
                Ok files ->
                    { model | files = fileStructureToFiles files model.sort model.sortedColumn } ! []

                _ ->
                    model ! []

        Open file ->
            case file of
                File file ->
                    model ! [ Navigation.load <| "/file/" ++ toString file.fileId ++ "/" ++ file.fileName ]

                Folder folder ->
                    model ! [ Navigation.newUrl <| "/main/" ++ toString folder.folderId ]

        FolderChainLoadResult res ->
            case res of
                Ok folders ->
                    { model | folderChain = folders } ! []

                _ ->
                    model ! []

        ChangeSort sortedColumn ->
            case sortedColumn == model.sortedColumn of
                True ->
                    model
                        ! [ Navigation.newUrl <|
                                String.concat
                                    [ "/main/"
                                    , toString model.parentId
                                    , "?sort="
                                    , sortToString <| sortNext model.sort
                                    , "&sortedColumn="
                                    , sortedColumnToString sortedColumn
                                    ]
                          ]

                False ->
                    model
                        ! [ Navigation.newUrl <|
                                String.concat
                                    [ "/main/"
                                    , toString model.parentId
                                    , "?sort="
                                    , sortToString Asc
                                    , "&sortedColumn="
                                    , sortedColumnToString sortedColumn
                                    ]
                          ]


view : Model -> Html Msg
view model =
    Helpers.viewLoggedIn model.helpers model.user HelpersMsg (viewContent model)


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
            let
                suffix =
                    file.fileName
                        |> String.split "."
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ""
            in
                case suffix of
                    "pdf" ->
                        "fa-file-pdf-o"

                    "png" ->
                        "fa-file-image-o"

                    "gif" ->
                        "fa-file-image-o"

                    "jpeg" ->
                        "fa-file-image-o"

                    "jpg" ->
                        "fa-file-image-o"

                    _ ->
                        "fa-file-o"

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
                [ model.folderChain
                    |> List.map (\folder -> span [ onClick <| Open <| Folder folder, class "pointer" ] [ text folder.folderName ])
                    |> List.intersperse (span [] [ i [ class "fa fa-caret-right", attribute "aria-hidden" "true" ] [] ])
                    |> h4 []
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
                        ]
                    }
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ h3 [] [ text <| Maybe.withDefault "" model.errorMsg ]
                , case model.files of
                    [] ->
                        text "Empty folder"

                    _ ->
                        Table.table
                            { options = [ Table.hover ]
                            , thead =
                                Table.simpleThead
                                    [ Table.th [ Table.cellAttr (class "w-75 pointer"), Table.cellAttr (onClick <| ChangeSort Name) ]
                                        [ text "Name "
                                        , span
                                            [ classList
                                                [ ( "fa fa-sort-desc", model.sort == Desc && model.sortedColumn == Name )
                                                , ( "fa fa-sort-asc", model.sort == Asc && model.sortedColumn == Name )
                                                ]
                                            , attribute "aria-hidden" "true"
                                            ]
                                            []
                                        ]
                                    , Table.th [ Table.cellAttr (class "w-25 pointer"), Table.cellAttr (onClick <| ChangeSort Date) ]
                                        [ text "Last modified "
                                        , span
                                            [ classList
                                                [ ( "fa fa-sort-desc", model.sort == Desc && model.sortedColumn == Date )
                                                , ( "fa fa-sort-asc", model.sort == Asc && model.sortedColumn == Date )
                                                ]
                                            , attribute "aria-hidden" "true"
                                            ]
                                            []
                                        ]
                                    , Table.th [ Table.cellAttr (class "w-10") ] []
                                    ]
                            , tbody =
                                Table.tbody [] <|
                                    List.indexedMap
                                        (\id ( file, dropdownState ) ->
                                            let
                                                link =
                                                    case file of
                                                        File file ->
                                                            "/file/" ++ toString file.fileId ++ "/" ++ file.fileName

                                                        Folder folder ->
                                                            "/main/" ++ toString folder.folderId
                                            in
                                                Table.tr []
                                                    [ Table.td [ Table.cellAttr <| class "pointer", Table.cellAttr <| onClick <| Open file ]
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
                                                                [ Dropdown.buttonItem [ class "pointer", onClick <| RenameFileInit file ] [ text "Rename" ]
                                                                , Dropdown.buttonItem [ class "pointer", onClick <| DeleteFile file ] [ text "Delete" ]
                                                                ]
                                                            }
                                                        ]
                                                    ]
                                        )
                                        model.files
                            }
                ]
            ]
        , Modal.config FileUploadModalMsg
            |> Modal.small
            |> Modal.h3 [] [ text "Upload file(s)" ]
            |> Modal.body []
                [ Html.form [ enctype "multipart/form-data", action <| "/fileUpload/" ++ toString model.parentId, method "POST" ]
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
        , Modal.config RenameFileModalMsg
            |> Modal.small
            |> Modal.h3 [] [ text "Rename file" ]
            |> Modal.body []
                [ text "File name"
                , Input.text [ Input.attrs [ onInput RenameFileNameSet, value model.renameFileName ] ]
                , text <| Maybe.withDefault "" model.renameFileError
                ]
            |> Modal.footer []
                [ Button.button [ Button.outlineSuccess, Button.attrs [ onClick RenameFileSubmit ] ] [ text "Submit" ] ]
            |> Modal.view model.renameFileModalState
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
