{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}

module App where

import           Control.Lens
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Data.Map
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Wai
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Heist
import           Data.HashMap.Lazy
import           Data.Aeson

import           Api
import           Models

import           Servant.API
import           Servant (serveSnap, Server, serveDirectory)


data App = App {
    _heist :: Snaplet (Heist App)
  , _sess  :: Snaplet SessionManager
  , _auth  :: Snaplet (AuthManager App)
  , _db    :: Snaplet Postgres
  }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

type AppHandler = Handler App App

testApi :: Proxy (Api AppHandler)
testApi = Proxy

-- withAssets :: Proxy WithAssets
-- withAssets = Proxy
app :: Server (Api AppHandler) '[] AppHandler
app = 
  (getFolderContents :<|> 
  renameFolder      :<|> 
  moveFolder        :<|> 
  deleteFolder      :<|> 
  addFolder         :<|> 
  getFolderChain    :<|> 

  renameFile        :<|>
  moveFile          :<|>
  deleteFile        :<|>

  user)

  :<|> 

  (testLogin         :<|> 
  login              :<|>
  register           :<|>
  logout          )where

  getFolderContents :: Int -> AppHandler FileStructure 
  getFolderContents parentId = do 
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> do 
        folders <- query 
          "SELECT * FROM folders WHERE folderParentId=? AND folderUid=?"
          (parentId, maybe "0" unUid $ userId u)
        files <- query 
          "SELECT * FROM files WHERE fileFolderId=? AND fileUid=?"
          (parentId, maybe "0" unUid $ userId u)
        return $ FileStructure (files :: [File]) (folders :: [Folder])
    

  renameFolder      :: Int -> Text -> AppHandler FileStructure
  renameFolder folderId newName = do
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> with db $ do 
        folderExists <- query 
          "SELECT * FROM folders WHERE folderId=? AND folderUid=?"
          (folderId, maybe "0" unUid $ userId u)
        case (folderExists :: [Folder]) of 
          [folder] -> do 
            otherNameExistsFolder <- query 
              "SELECT * FROM folders WHERE folderParentId=? AND folderName=?"
              (folderParentId folder, newName)
            otherNameExistsFile <- query 
              "SELECT * FROM files WHERE fileFolderId=? AND fileName=?"
              (folderParentId folder, newName)
            case (otherNameExistsFolder :: [Folder], otherNameExistsFile :: [File]) of 
              ([], []) -> do 
                xs <- (query
                  "UPDATE folders SET folderName=? WHERE folderId=? RETURNING folderId"
                  (newName, folderId) :: Handler App Postgres [Only Int])            
                getFileStructure $ folderParentId folder 

              _ -> fail"file name exists"  

          _ -> fail "no folder"

  moveFolder        :: Int -> Int -> AppHandler FileStructure
  moveFolder        = 
    undefined 

  deleteFolder      :: Int -> AppHandler FileStructure
  deleteFolder      folderId = do
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> with db $ do 
        folderExists <- (query 
          "SELECT * FROM folders WHERE folderId=? AND folderUid=?"
          (folderId, maybe "0" unUid $ userId u) :: Handler App Postgres [Folder])
        case folderExists of 
          [folder] -> do 
            xs <- (query
              "DELETE FROM folders WHERE folderId=?"
              ([folderId]) :: Handler App Postgres [Only Int])            
            getFileStructure $ folderParentId folder 
          _ -> fail "folder does not exist"


  addFolder      :: Int -> Text -> AppHandler FileStructure
  addFolder parentId folderName =  do  
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> with db $ do 
        parentExist <- (query 
          "SELECT * FROM folders WHERE folderId=? AND folderUid=?"
          (parentId, maybe "0" unUid $ userId u) :: Handler App Postgres [Folder])
        parentExist' <- case parentId == 0 of 
          True -> return True 
          False -> return $ Prelude.length parentExist == 1
        sameFolderName <- (query 
          "SELECT * FROM folders WHERE folderParentId=? AND folderName=?"
          (parentId, folderName) :: Handler App Postgres [Folder])
        sameFileName <- (query 
          "SELECT * FROM files WHERE fileFolderId=? AND fileName=?"
          (parentId, folderName) :: Handler App Postgres [File])
        case (parentExist', sameFolderName, sameFileName) of 
          (True, [], []) -> do 
            xs <- (query
              "INSERT INTO folders (folderParentId, folderName, folderUid, folderInsertDate) VALUES (?,?,?,NOW()) RETURNING folderId"
              (parentId, folderName, maybe "0" unUid $ userId u) :: Handler App Postgres [Only Int]) 
            getFileStructure parentId 
   
          _ -> fail "name exists"

  getFolderChain    :: Int -> AppHandler [Folder]
  getFolderChain folderId = do 
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> with db $ do 
        folderExists <- (query 
          "SELECT * FROM folders WHERE folderId=? AND folderUid=?"
          (folderId, maybe "0" unUid $ userId u) :: Handler App Postgres [Folder])
        case folderExists of 
          [folder] -> do 
            getChain [folder] $ folderParentId folder
          _ -> case folderId of 
            0 -> return []
            _ -> fail "folder does not exist"

    where 
      getChain :: [Folder] -> Int -> Handler App Postgres [Folder]
      getChain folders 0 = return folders 
      getChain folders parentId = do 
        folderExists <- (query 
          "SELECT * FROM folders WHERE folderId=?"
          ([parentId]) :: Handler App Postgres [Folder])
        case folderExists of 
          [folder] -> getChain (folder : folders) $ folderParentId folder
          _ -> fail "no parent"


  renameFile        :: Int -> Text -> AppHandler FileStructure
  renameFile fileId newName = do
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> fail "no user"
      Just u -> with db $ do 
        fileExists <- query 
          "SELECT * FROM files WHERE fileId=? AND fileUid=?"
          (fileId, maybe "0" unUid $ userId u)
        case (fileExists :: [File]) of 
          [file] -> do 
            otherNameExistsFolder <- query 
              "SELECT * FROM folders WHERE folderParentId=? AND folderName=?"
              (fileFolderId file, newName)
            otherNameExistsFile <- query 
              "SELECT * FROM files WHERE fileFolderId=? AND fileName=?"
              (fileFolderId file, newName)
            case (otherNameExistsFolder :: [Folder], otherNameExistsFile :: [File]) of 
              ([], []) -> do 
                xs <- (query
                  "UPDATE files SET fileName=? WHERE fileId=? RETURNING fileId"
                  (newName, fileId) :: Handler App Postgres [Only Int])            
                getFileStructure $ fileFolderId file

              _ -> fail "file name exists"  

          _ -> fail "no folder"

  moveFile          :: Int -> Int -> AppHandler FileStructure
  moveFile          = 
    undefined 

  deleteFile        :: Int -> AppHandler FileStructure
  deleteFile        = 
    undefined 

  user              :: User -> AppHandler (Maybe User)
  user userData     = with auth $ do 
    cu <- currentUser 
    case cu of 
      Nothing -> return Nothing 
      Just u -> do  
        result <- saveUser $ u { Snap.Snaplet.Auth.userLogin = Models.userLogin userData, 
                                 Snap.Snaplet.Auth.userPassword = Just $ ClearText $ encodeUtf8 $ Models.userPassword userData,
                                 -- hack because userMeta does not work with postgres backend
                                 userEmail = Just $ pack $ show $ Models.userPreference userData
                                }
        case result of 
          Right cu' -> return $ Just $ toUser cu'
          Left _ -> return Nothing 

  testLogin         :: AppHandler (Maybe User)
  testLogin          = undefined

  login             :: LoginForm -> AppHandler (Maybe User)
  login loginForm    = undefined

  register          :: LoginForm -> AppHandler (Maybe User)
  register loginForm = undefined

  logout            :: AppHandler ()
  logout             = undefined


getFileStructure :: Int -> Handler App Postgres FileStructure
getFileStructure parentId = do 
  folders <- query 
    "SELECT * FROM folders WHERE folderParentId=?"
    ([parentId])
  files <- query 
    "SELECT * FROM files WHERE fileFolderId=?"
    ([parentId])
  return $ FileStructure (files :: [File]) (folders :: [Folder])
