{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson
import Data.Text

import GHC.Generics
import Database.Persist.TH
import           Servant.Elm      (ElmType)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  password  Int
  deriving Eq Read Show Generic 
Folder
  userid Int 
  parentid Int 
  name Text 
  deriving Eq Read Show Generic
File
  userid Int 
  folderid Int 
  name Text
  deriving Eq Read Show Generic
|]

instance FromJSON User 
instance ToJSON User 
instance ElmType User

instance FromJSON Folder
instance ToJSON Folder
instance ElmType Folder

instance FromJSON File
instance ToJSON File
instance ElmType File

-- data FileStructure = Single File | Multiple Folder [FileStructure]
--   deriving (Eq, Read, Show, Generic)


data FileStructure = FileStructure {files :: [File], folders :: [Folder]} deriving (Eq, Read, Show, Generic)


instance FromJSON FileStructure
instance ToJSON FileStructure
instance ElmType FileStructure