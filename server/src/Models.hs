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
{-# LANGUAGE RecordWildCards            #-}

module Models where

import           Data.Aeson
import           Data.Text
import           Data.Text.Read

import           GHC.Generics
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth
import           Database.PostgreSQL.Simple.ToField
import           Servant.Elm      (ElmType)

import           Data.HashMap.Lazy
import           Data.Time.Clock 

data Folder = Folder 
  { folderId :: Int
  , folderParentId :: Int 
  , folderName :: Text 
  , folderUid :: Text
  , folderInsertDate :: UTCTime
  } deriving (Eq, Show, Read, Generic)

data File = File 
  { fileId :: Int
  , fileFolderId :: Int 
  , fileName :: Text 
  , fileUid :: Text
  , fileSize :: Int 
  , fileInsertDate :: UTCTime
  } deriving (Eq, Show, Read, Generic)

data User = User 
  { userLogin :: Text
  , userUid :: Text
  , userPassword :: Text 
  , userPreference :: Int 
  } deriving (Eq, Show, Read, Generic) 

data LoginForm = LoginForm 
  { username :: Text
  , password :: Text 
  } deriving (Eq, Show, Read, Generic) 

instance FromJSON User 
instance ToJSON User 
instance ElmType User

instance FromJSON LoginForm 
instance ToJSON LoginForm 
instance ElmType LoginForm

result :: a -> Result a -> a 
result def res = 
  case res of 
    Success a -> a 
    _ -> def

toUser :: AuthUser -> User 
toUser AuthUser{..} = 
  User userLogin (maybe "0" unUid userId) "" preference
  where 
    preference :: Int 
    preference = case Data.Text.Read.decimal $ maybe "" id userEmail of 
      Right (x, _) -> x 
      _ -> 0 

instance FromJSON Folder
instance ToJSON Folder
instance ElmType Folder
instance FromRow Folder where 
  fromRow = Folder <$> field <*> field <*> field <*> field <*> field 
instance ToRow Folder where 
  toRow Folder{..} = 
    [ toField folderId
    , toField folderParentId
    , toField folderName
    , toField folderUid
    , toField folderInsertDate
    ] 

instance FromJSON File
instance ToJSON File
instance ElmType File
instance FromRow File where 
  fromRow = File <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow File where 
  toRow File{..} = 
    [ toField fileId
    , toField fileFolderId
    , toField fileName
    , toField fileUid
    , toField fileSize
    , toField fileInsertDate
    ] 

data FileStructure = FileStructure {files :: [File], folders :: [Folder]} deriving (Eq, Read, Show, Generic)


instance FromJSON FileStructure
instance ToJSON FileStructure
instance ElmType FileStructure
