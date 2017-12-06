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

import           Servant.API
import           Servant (serveSnap, Server, serveDirectory)

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi m =

  -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet


  :<|> "hellosnap" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

  -- POST /greet with a Greet as JSON in the request body,
  --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

  :<|> "new" :> Capture "name" Text :> Get '[JSON] Greet

  :<|> "tryLogin" :> Capture "name" Text :> Get '[JSON] Greet

  :<|> "all" :> Get '[JSON] [AuthUser]

  -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] ()

  :<|> "files" :> Raw
  :<|> "doraw" :> Raw


-- Our application has some of the usual Snaplets
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

testApi :: Proxy (TestApi AppHandler)
testApi = Proxy


-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'AppHandler' monad.

server :: Server (TestApi AppHandler) '[] AppHandler
server = helloH
    :<|> helloH'
    :<|> postGreetH
    :<|> newUser
    :<|> tryLogin
    :<|> allUsers
    :<|> deleteGreetH
    :<|> serveDirectory "static"
    :<|> doRaw

  where helloH :: Text -> Maybe Bool -> AppHandler Greet
        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        helloH' :: Text -> Maybe Bool -> (Handler App App) Greet
        helloH' name _ = with auth $ do
          cu <- loginByRememberToken
          withTop sess $ getFromSession "test" >>= liftIO . putStrLn . show 
          return (Greet $ "Hi from snaplet, " <> name
                  <> ". Login is " <> (pack . show) cu)

        postGreetH :: Greet -> (Handler App App) Greet
        postGreetH greet = return greet

        deleteGreetH _ = return ()

        doRaw = with auth $ do
          u <- currentUser
          let spl = "tName" ## I.textSplice (maybe "NoLogin" (pack . show) u)
          renderWithSplices "test" spl

        newUser :: Text -> (Handler App App) Greet
        newUser name = with auth $ do 
            u <- createUser name "" >>= \u -> case u of
                Left _   -> return u
                Right u' -> saveUser (u' {userEmail = Data.Text.Encoding.decodeUtf8 <$>  (Just "@email")})
            return $ Greet "added"

        tryLogin :: Text -> (Handler App App) Greet
        tryLogin name = with auth $ do 
            u <- loginByUsername name (ClearText "") True 
            cu <- loginByRememberToken 
            liftIO $ putStrLn $ show cu
            withTop sess $ setInSession "test" "asd"
            withTop sess $ getFromSession "test" >>= liftIO . putStrLn . show 
            case u of 
                Right user -> return $ Greet $ pack $ show user
                Left failure -> return $ Greet $ pack $ show failure 

        allUsers :: (Handler App App) [AuthUser]
        allUsers = do
            results <- query_ "select * from snap_auth_user"
            return $ (results :: [AuthUser]) 


initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "An example app in servant" Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "_bbsession" (Just "test") (Just 3600)
  d <- nestSnaplet "db" db pgsInit
  a <- nestSnaplet "" auth $ initPostgresAuth sess d
  addRoutes [("testCreate", testCreate)
            ,("testLogin", testLogin)
            ,("api", withSession sess $ serveSnap testApi server)
            ,("",    writeText "Hello")]
  return $ App h s a d 

testLogin :: (Handler App App) () 
testLogin = with auth $ do 
  cu <- loginByRememberToken
  liftIO $ putStrLn $ show $ cu 
  return ()

testCreate :: (Handler App App) () 
testCreate = with auth $ do 
  cu <- loginByUsername "asd" (ClearText "") True
  liftIO $ putStrLn $ show $ cu 
  return ()


-- Run the server.
main :: IO ()
main = serveSnaplet defaultConfig initApp

