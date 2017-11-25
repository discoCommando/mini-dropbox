{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Servant.Elm
import           Data.Text hiding (intercalate, map)
import           Elm (toElmDecoderSource, toElmEncoderSource, toElmTypeSource)

import           Models
import           Api


main :: IO ()
main = do
  let code = "module Api exposing (..)" :
            defElmImports :
            toElmTypeSource (Proxy :: Proxy Item) :
            toElmDecoderSource (Proxy :: Proxy Item) :
            toElmTypeSource (Proxy :: Proxy File) :
            toElmDecoderSource (Proxy :: Proxy File) :
            toElmTypeSource (Proxy :: Proxy Folder) :
            toElmDecoderSource (Proxy :: Proxy Folder) :
            toElmTypeSource (Proxy :: Proxy FileStructure) :
            toElmDecoderSource (Proxy :: Proxy FileStructure) :
            generateElmForAPI api
  writeFile "client/Api.elm" $ intercalate "\n\n" $ map unpack code  

