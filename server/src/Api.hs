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


module Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Elm      (ElmType)
import           Data.Text
import           Models

type Paths m = 
  --test 
  "folder"  :> Capture "folderid" Int                               :> Get '[JSON] FileStructure    :<|>
  "folder"  :> Capture "folderid" Int :> ReqBody '[JSON] Text       :> Post '[JSON] FileStructure   :<|>
  "folder"  :> Capture "folderid" Int :> ReqBody '[JSON] Int        :> Post '[JSON] FileStructure   :<|>
  "folder"  :> Capture "folderid" Int                               :> Delete '[JSON] FileStructure :<|>
  -- "folder"  :> Capture "folderid" Int :> ReqBody '[JSON] Text       :> Put '[JSON] FileStructure    :<|>

  "file"    :> Capture "fileid" Int   :> ReqBody '[JSON] Text       :> Post '[JSON] FileStructure   :<|>
  "file"    :> Capture "fileid" Int   :> ReqBody '[JSON] Int        :> Post '[JSON] FileStructure   :<|>
  "file"    :> Capture "fileid" Int                                 :> Delete '[JSON] FileStructure 


-- dummy paths that are handled in main, not by servant, but they will be helpful for generating elm code 
type DummyPaths m = 
  "testLogin" :>                              Get '[JSON] (Maybe User) :<|>
  "login"     :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Maybe User):<|>
  "register"  :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Maybe User)

type Api m = "api" :> Paths m :<|> DummyPaths m 

-- type Api = 
--   "api" :> ("files" :> Capture "folderid" Int :> Get '[JSON] FileStructure :<|>
--   "item" :> Get '[JSON] [ItemId] :<|>
--   "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
--   "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
--   "item" :> Capture "itemId" ItemId :> Delete '[JSON] ItemId)


-- api :: Proxy Api
-- api = Proxy

-- types

-- type ItemId = Int

-- data Item
--   = Item {
--     id :: ItemId,
--     text :: String
--   }
--   deriving (Show, Eq, Generic)

-- instance ElmType Item
-- instance ToJSON Item
-- instance FromJSON Item
