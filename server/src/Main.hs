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
            ,("testLogin", testLogin)
            ,("testCreate", testCreate)
            ,("", withSession sess $ serveSnap testApi app)
            ,("", serveFile "assets/index.html")]
  -- wrapHandlers tryLogin 
  return $ App h s a d 

testLogin :: (Handler App App) ()
testLogin = with auth $ do 
  cu <- currentUser
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ fmap toUser cu
  -- return "test"
  
testCreate :: (Handler App App) () 
testCreate = with auth $ do 
  u <- createUser "asd" "" >>= \u -> case u of
                Left _   -> return u
                Right u' -> saveUser u'
  liftIO $ putStrLn $ show $ u
  return ()

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
      -- cu <- loginByUsername (username loginForm) (ClearText $ encodeUtf8 $ password loginForm) True 
      case u of 
        Right user -> writeLBS . encode $ fmap toUser (Just user)
        Left _ -> writeLBS . encode $ (Nothing :: Maybe User)
    _ -> 
      writeLBS . encode $ (Nothing :: Maybe User)

-- testCreate :: (Handler App App) () 
-- testCreate = with auth $ do 
--   cu <- loginByUsername "asd" (ClearText "") True
--   liftIO $ putStrLn $ show $ cu 
--   return ()


-- Run the server.
main :: IO ()
main = serveSnaplet defaultConfig initApp

