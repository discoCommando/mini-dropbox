{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Servant.Elm
import           Data.Text hiding (intercalate, map)
import           Elm (toElmDecoderSource, toElmEncoderSource, toElmTypeSource)

import           Models
import           Api

api :: Proxy (Api ())
api = Proxy 

main :: IO ()
main = do
  let code = "module Api exposing (..)" :
            defElmImports :
            "import Date exposing (Date)" :
            "decodeDate : Decoder Date" :
            "decodeDate = " : 
            "       string |> Json.Decode.andThen (\\s -> Date.fromString s |> Result.toMaybe |> Maybe.map Json.Decode.succeed |> Maybe.withDefault (Json.Decode.fail \"not a date\"))" :
            toElmTypeSource (Proxy :: Proxy File) :
            toElmDecoderSource (Proxy :: Proxy File) :
            toElmTypeSource (Proxy :: Proxy Folder) :
            toElmDecoderSource (Proxy :: Proxy Folder) :
            toElmTypeSource (Proxy :: Proxy FileStructure) :
            toElmDecoderSource (Proxy :: Proxy FileStructure) :
            toElmTypeSource (Proxy :: Proxy User) :
            toElmDecoderSource (Proxy :: Proxy User) :
            toElmTypeSource (Proxy :: Proxy LoginForm) :
            toElmDecoderSource (Proxy :: Proxy LoginForm) :
            toElmEncoderSource (Proxy :: Proxy LoginForm) :
            generateElmForAPI api
  writeFile "client/Api.elm" $ intercalate "\n\n" $ map unpack code  

