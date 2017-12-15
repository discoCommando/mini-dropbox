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
  "folder"              :> Capture "folderid" Int                               :> Get '[JSON] FileStructure    :<|>
  "folder"  :> "rename" :> Capture "folderid" Int :> ReqBody '[JSON] Text       :> Post '[JSON] FileStructure   :<|>
  "folder"              :> Capture "folderid" Int                               :> Delete '[JSON] FileStructure :<|>
  "folder"              :> Capture "folderid" Int :> ReqBody '[JSON] Text       :> Put '[JSON] FileStructure    :<|>
  "folder"  :> "chain"  :> Capture "folderid" Int                               :> Get '[JSON] [Folder]         :<|>

  "file"    :> "rename" :> Capture "fileid" Int   :> ReqBody '[JSON] Text       :> Post '[JSON] FileStructure   :<|>
  "file"                :> Capture "fileid" Int                                 :> Delete '[JSON] FileStructure :<|>

  "user"                                          :> ReqBody '[JSON] User       :> Post '[JSON] (Maybe User)    :<|>

  "capacity"                                                                    :> Get '[JSON] Int


-- dummy paths that are handled in main, not by servant, but they will be helpful for generating elm code 
type DummyPaths m = 
  "testLogin" :>                              Post '[JSON] (Maybe User) :<|>
  "login"     :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Maybe User):<|>
  "register"  :> ReqBody '[JSON] LoginForm :> Post '[JSON] (Maybe User):<|>
  "logout"    :>                              Post '[JSON] ()

type Api m = "api" :> Paths m :<|> DummyPaths m 
