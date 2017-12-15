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

import           Control.Lens
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson hiding (defaultOptions)
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Heist.Interpreted as I
import           Snap.Core
import           Snap.CORS
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session

import qualified Database.PostgreSQL.Simple as P
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Http.Server (defaultConfig)
import           Snap.Util.FileServe

import           Servant.API
import           Servant (serveSnap, Server, serveDirectory)
import           Models 
import           App
import           Data.HashMap.Lazy
import           Snap.Util.FileUploads
import           System.Posix          (FileOffset, fileSize, getFileStatus)
import           System.Directory 

-- * Example

-- | A greet message data type

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "_session" (Just "test") (Just 3600)
  d <- nestSnaplet "db" db pgsInit
  a <- nestSnaplet "" auth $ initPostgresAuth sess d
  addRoutes [("assets", Snap.Util.FileServe.serveDirectory "assets")
            ,("login", login)
            ,("register", register)
            ,("logout", Main.logout)
            ,("testLogin", testLogin)
            ,("fileUpload/:folderId", fileUpload)
            ,("file/:fileId/:fileName", fileServe)
            ,("", withSession sess $ serveSnap testApi app)
            ,("", serveFile "assets/index.html")]
  -- wrapHandlers tryLogin 
  return $ App h s a d 

testLogin :: (Handler App App) ()
testLogin = with auth $ do 
  cu <- currentUser
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ fmap toUser cu

tryLogin :: (Handler App App) () 
tryLogin = with auth $ do 
  cu <- loginByRememberToken
  liftIO $ putStrLn $ show $ cu 
  return ()

login :: (Handler App App) () 
login = do 
  reqBody <- readRequestBody 2048 
  result <- return (Data.Aeson.decode reqBody :: Maybe LoginForm)
  modifyResponse $ setHeader "Content-Type" "application/json"
  case result of 
    Just loginForm -> with auth $ do
      cu <- loginByUsername (username loginForm) (ClearText $ encodeUtf8 $ password loginForm) True 
      case cu of 
        Right user -> writeLBS . encode $ fmap toUser (Just user)
        Left _ -> writeLBS . encode $ (Nothing :: Maybe User)
    _ -> 
      writeLBS . encode $ (Nothing :: Maybe User)

logout :: (Handler App App) () 
logout = with auth $ do 
  Snap.Snaplet.Auth.logout 
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ ()

register :: (Handler App App) () 
register = do 
  reqBody <- readRequestBody 2048 
  result <- return (Data.Aeson.decode reqBody :: Maybe LoginForm)
  modifyResponse $ setHeader "Content-Type" "application/json"
  case result of 
    Just LoginForm{..} -> with auth $ do
      u <- createUser username (encodeUtf8 password) >>= \u -> case u of
                Left _   -> return u
                Right u' -> saveUser u'
      case u of 
        Right user -> writeLBS . encode $ fmap toUser (Just user)
        Left _ -> writeLBS . encode $ (Nothing :: Maybe User)
    _ -> 
      writeLBS . encode $ (Nothing :: Maybe User)



-- getFileSize :: FilePath -> IO FileOffset
-- getFileSize path = System.Posix.fileSize <$> getFileStatus path

fileUpload :: (Handler App App) () 
fileUpload = do 
  cu <- with auth $ currentUser
  folderIdS <- getParam "folderId"
  case (cu, folderIdS) of 
    (Nothing, _) -> fail "no User"
    (_, Nothing) -> fail "no folder"
    (Just u, Just f) -> with db $ do 
      let folderId = read (B8.unpack f) :: Int 
      folderExists <- (query 
          "SELECT * FROM folders WHERE folderId=? AND folderUid=?"
          (folderId, maybe "0" unUid $ userId u) :: Handler App Postgres [Folder]) 
      folderExists' <- case folderId == 0 of 
        True -> return True 
        False -> return $ Prelude.length folderExists == 1
      case folderExists' of 
        False -> fail "wrong folder id"
        True -> do 
          l <- handleFileUploads "tmp" defaultUploadPolicy
               (const $ allowWithMaximumSize (getMaximumFormInputSize defaultUploadPolicy))
               (\pinfo mbfname -> do 
                  fsize <- either (const $ return 0) getFileSize mbfname
                  case (partFileName pinfo, mbfname) of 
                    (Just name, Right pathName) -> do 
                      case fsize of 
                        0 -> return Nothing 
                        _ -> do 
                          System.Directory.renameFile pathName ("tmp2/" ++ getName pathName)
                          return $ Just (name, fsize, "tmp2/" ++ getName pathName)
                    _ -> 
                      return Nothing 
                )
          let justs = getJusts l 
          let allSize = Prelude.sum $ Prelude.map (\(a, b, c) -> b) justs 
          --TODO check size 
          fileNames <- forM justs $ \(name, fsize, path) -> do 
            sameFileName <- (query 
              "SELECT * FROM files WHERE fileFolderId=? AND fileName=?"
              (folderId, name) :: Handler App Postgres [File])
            sameFolderName <- (query 
              "SELECT * FROM folders WHERE folderParentId=? AND folderName=?"
              (folderId, name) :: Handler App Postgres [Folder])
            return $ (sameFileName, sameFolderName) == ([], [])

          let allEmpty = allTrue fileNames
          case allEmpty of 
            False -> do
              forM justs $ \(_, _, path) -> do 
                liftIO $ removeFile path
              redirect $ B8.concat ["/main/", (B8.pack $ show folderId), "?error=1"]
            True -> do 
              forM justs $ \(name, fsize, path) ->  do 
                [Only fileId] <- (query
                  "INSERT INTO files (fileFolderId, fileName, fileUid, fileInsertDate, fileSize) VALUES (?,?,?,NOW(),?) RETURNING fileId"
                  (folderId, name, maybe "0" unUid $ userId u, fsize) :: Handler App Postgres [Only Int]) 
                liftIO $ renameFile path ("files/" ++ show fileId)

              redirect $ B8.append "/main/" (B8.pack $ show folderId)

   where 

    getJusts [] = []
    getJusts (Just a : rest) = a : getJusts rest
    getJusts (Nothing : rest) = getJusts rest 

    getName = Prelude.head . Prelude.reverse . Prelude.words . Prelude.map (\s -> if s == '/' then ' ' else s) 

    allTrue [] = True 
    allTrue (True : rest) = allTrue rest 
    allTrue (False : _) = False 

fileServe :: (Handler App App) ()
fileServe = do 
  cu <- with auth $ currentUser
  fileIdS <- getParam "fileId"
  case (cu, fileIdS) of 
    (Nothing, _) -> fail "no user"
    (_, Nothing) -> fail "no file"
    (Just u, Just f) -> with db $ do 
      let fileId' = read (B8.unpack f) :: Int 
      fileExists <- (query 
        "SELECT * FROM files WHERE fileId=? AND fileUid=?"
        (fileId', maybe "0" unUid $ userId u) :: Handler App Postgres [File])

      liftIO $ putStrLn $ show fileExists
      case fileExists of 
        [file] -> do
          serveFile ("files/"++ show fileId')
        _ -> fail "no file" 


-- Run the server.
main :: IO ()
main = serveSnaplet defaultConfig initApp

