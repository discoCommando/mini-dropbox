module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Item =
    { id : Int
    , text : String
    }


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "id" int
        |> required "text" string


type alias File =
    { fileUserid : Int
    , fileFolderid : Int
    , fileName : String
    }


decodeFile : Decoder File
decodeFile =
    decode File
        |> required "fileUserid" int
        |> required "fileFolderid" int
        |> required "fileName" string


type alias Folder =
    { folderUserid : Int
    , folderParentid : Int
    , folderName : String
    }


decodeFolder : Decoder Folder
decodeFolder =
    decode Folder
        |> required "folderUserid" int
        |> required "folderParentid" int
        |> required "folderName" string


type alias FileStructure =
    { files : List File
    , folders : List Folder
    }


decodeFileStructure : Decoder FileStructure
decodeFileStructure =
    decode FileStructure
        |> required "files" (list decodeFile)
        |> required "folders" (list decodeFolder)


getApiFilesByFolderid : Int -> Http.Request FileStructure
getApiFilesByFolderid capture_folderid =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "files"
                , capture_folderid |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeFileStructure
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiItem : Http.Request (List Int)
getApiItem =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiItemByItemId : Int -> Http.Request Item
getApiItemByItemId capture_itemId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeItem
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postApiItem : String -> Http.Request Int
postApiItem body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }


deleteApiItemByItemId : Int -> Http.Request Int
deleteApiItemByItemId capture_itemId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }
