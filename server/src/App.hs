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
  getFolderContents id = 
    return $ FileStructure [] []

  renameFolder      :: Int -> Text -> AppHandler FileStructure
  renameFolder      = 
    undefined 

  moveFolder        :: Int -> Int -> AppHandler FileStructure
  moveFolder        = 
    undefined 

  deleteFolder      :: Int -> AppHandler FileStructure
  deleteFolder      = 
    undefined 

  addFolder      :: Int -> Text -> AppHandler FileStructure
  addFolder parentId folderName =  do  
    cu <- with auth $ currentUser
    case cu of 
      Nothing -> return $ FileStructure [] []
      Just u -> with db $ do 
        xs <- (query
          "INSERT INTO folders (folderParentId, folderName, folderUid, folderInsertDate) VALUES (?,?,?,NOW()) RETURNING folderId"
          (parentId, folderName, maybe "0" unUid $ userId u) :: Handler App Postgres [Only Int]) 
        folders <- query 
          "SELECT * FROM folders WHERE folderParentId=?"
          ([parentId])
        files <- query 
          "SELECT * FROM files WHERE fileFolderId=?"
          ([parentId])
        return $ FileStructure (files :: [File]) (folders :: [Folder])


  renameFile        :: Int -> Text -> AppHandler FileStructure
  renameFile        = 
    undefined 

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



