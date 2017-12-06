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
import           GHC.Generics
import           Data.Map
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Wai
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Heist

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
app = getFolderContents where 
  getFolderContents :: Int -> AppHandler FileStructure 
  getFolderContents id = 
    return $ FileStructure [] []


-- server :: IO (Server WithAssets)
-- server = do
--   assets <- serveAssets
--   db <- mkDB
--   return $ apiServer db :<|> serveDirectory "assets/index.html"

-- apiServer :: DB -> Server Api
-- apiServer db =
--   listItems db :<|>
--   getItem db :<|>
--   postItem db :<|>
--   deleteItem db

-- apiServer :: DB -> Server Api
-- apiServer db =
--   listFiles db :<|>
--   listItems db :<|>
--   getItem db :<|>
--   postItem db :<|>
--   deleteItem db

-- listFiles :: DB -> Int -> Handler FileStructure
-- listFiles db id = 
--   return $ FileStructure [File 1 id "test"] []

-- listItems :: DB -> Handler [ItemId]
-- listItems db = liftIO $ allItemIds db

-- getItem :: DB -> ItemId -> Handler Item
-- getItem db n = maybe (throwE err404) return =<< liftIO (lookupItem db n)

-- postItem :: DB -> String -> Handler ItemId
-- postItem db new =
--   liftIO $ insertItem db new

-- -- fake DB

-- data DB = DB (MVar (Map ItemId String))

-- debug :: DB -> IO ()
-- debug (DB mvar) = readMVar mvar >>= print

-- mkDB :: IO DB
-- mkDB = DB <$> newMVar empty

-- insertItem :: DB -> String -> IO ItemId
-- insertItem (DB mvar) new = modifyMVar mvar $ \ m -> do
--   let newKey = case keys m of
--         [] -> 0
--         ks -> succ (maximum ks)
--   return (insert newKey new m, newKey)

-- lookupItem :: DB -> ItemId -> IO (Maybe Item)
-- lookupItem (DB mvar) i = do
--   fmap (Item i) <$> Data.Map.lookup i <$> readMVar mvar

-- allItemIds :: DB -> IO [ItemId]
-- allItemIds (DB mvar) =
--   keys <$> readMVar mvar

-- deleteItem :: MonadIO m => DB -> ItemId -> m ItemId
-- deleteItem (DB mvar) i = liftIO $ modifyMVar mvar $ \ m -> do
--   return (delete i m, i)
