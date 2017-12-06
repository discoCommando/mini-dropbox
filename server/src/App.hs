{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds #-}

module App where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Map
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant.Utils.StaticFiles
import           Servant

import           Api
import           Models

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app =
  serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  db <- mkDB
  return $ apiServer db :<|> serveDirectory "assets/index.html"

-- apiServer :: DB -> Server Api
-- apiServer db =
--   listItems db :<|>
--   getItem db :<|>
--   postItem db :<|>
--   deleteItem db

apiServer :: DB -> Server Api
apiServer db =
  listFiles db :<|>
  listItems db :<|>
  getItem db :<|>
  postItem db :<|>
  deleteItem db

listFiles :: DB -> Int -> Handler FileStructure
listFiles db id = 
  return $ FileStructure [File 1 id "test"] []

listItems :: DB -> Handler [ItemId]
listItems db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwE err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new =
  liftIO $ insertItem db new

-- fake DB

data DB = DB (MVar (Map ItemId String))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \ m -> do
  let newKey = case keys m of
        [] -> 0
        ks -> succ (maximum ks)
  return (insert newKey new m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = do
  fmap (Item i) <$> Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) =
  keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m ItemId
deleteItem (DB mvar) i = liftIO $ modifyMVar mvar $ \ m -> do
  return (delete i m, i)
