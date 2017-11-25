{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}

module Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Elm      (ElmType)
import           Database.Persist
import           Data.Text
import           Models

-- type Api =
--   "api" :>
--     ("item" :> Get '[JSON] [ItemId] :<|>
--      "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
--      "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
--      "item" :> Capture "itemId" ItemId :> Delete '[JSON] ItemId)



type Api = 
  "api" :> ("files" :> Capture "folderid" Int :> Get '[JSON] FileStructure :<|>
  "item" :> Get '[JSON] [ItemId] :<|>
  "item" :> Capture "itemId" ItemId :> Get '[JSON] Item :<|>
  "item" :> ReqBody '[JSON] String :> Post '[JSON] ItemId :<|>
  "item" :> Capture "itemId" ItemId :> Delete '[JSON] ItemId)


api :: Proxy Api
api = Proxy

-- types

type ItemId = Int

data Item
  = Item {
    id :: ItemId,
    text :: String
  }
  deriving (Show, Eq, Generic)

instance ElmType Item
instance ToJSON Item
instance FromJSON Item
