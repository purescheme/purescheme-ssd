-- Copyright 2020 Fernando Rincon Martin
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}
module Network.Wai.SSDom.Forms
  ( formsApp
  )
where

import Network.Wai.SSDom
import Purescheme.SSDom
import Text.XML.Simple

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Routing.Purescheme.Core
import Text.XML

jsVaadinName :: Text
jsVaadinName = T.concat ["vaadin-", T.decodeUtf8 $ B16.encode $ hash jsVaadin, ".js"]

jsVaadin :: ByteString
jsVaadin = $(embedFile "js/dist/vaadin.js")

formsApp :: Text -> IO SSDomIO -> SessionStorage -> Application
formsApp title createNetwork sessionStorage = 
  alternatives
    [ path jsVaadinName $ pathEnd $ getJsVaadin
    , appWithHeadNodes (headNodes title) createNetwork sessionStorage
    ]
  where
    
    getJsVaadin = complete $ responseLBS status200 [] (LBS.fromStrict jsVaadin)

headNodes :: Text -> [Node]
headNodes title = 
  [ NodeElement $ leafElement "meta" ! attribute "charset" "utf-8"
  , simpleNodeElement "title" title
  , NodeElement $ leafElement "script" ! attribute "src" jsVaadinName
  , simpleNodeElement "style" "body { font-family: var(--lumo-font-family); }"
  ]
