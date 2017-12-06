{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Servant.Elm
import           Data.Text hiding (intercalate, map)
import           Elm (toElmDecoderSource, toElmEncoderSource, toElmTypeSource)

import           Models
import           App


main :: IO ()
main = do
  let code = "module Api exposing (..)" :
            defElmImports :
            toElmTypeSource (Proxy :: Proxy File) :
            toElmDecoderSource (Proxy :: Proxy File) :
            toElmTypeSource (Proxy :: Proxy Folder) :
            toElmDecoderSource (Proxy :: Proxy Folder) :
            toElmTypeSource (Proxy :: Proxy FileStructure) :
            toElmDecoderSource (Proxy :: Proxy FileStructure) :
            toElmTypeSource (Proxy :: Proxy User) :
            toElmDecoderSource (Proxy :: Proxy User) :
            generateElmForAPI testApi
  writeFile "client/Api.elm" $ intercalate "\n\n" $ map unpack code  

