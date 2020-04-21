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
module Network.Wai.SSDom
  ( app
  , appWithHeadNodes
  , defaultHeadNodes
  , module Network.Wai.SSDom.Session
  )
where

import Network.Wai.SSDom.Session
import Purescheme.SSDom
import Text.XML.Simple
import Text.XML.Delta

import Control.Monad.STM (atomically, check)
import Control.Concurrent.STM.TVar (registerDelay, readTVar)
import Crypto.Hash.MD5 (hash)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedFile)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Routing.Purescheme.Core
import Text.Blaze (toMarkup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.XML

eventsDelay :: Int
eventsDelay = 60 * 1_000_000

firstLoadTimeout :: Int
firstLoadTimeout = 400_000

jsDriverName :: Text
jsDriverName = T.concat ["ssd-driver-", T.decodeUtf8 $ B16.encode $ hash jsDriver, ".js"]

jsDriver :: ByteString
jsDriver = $(embedFile "js/dist/ssd-driver.js")

defaultHeadNodes :: [Node]
defaultHeadNodes =
  [ NodeElement $ leafElement "meta" ! attribute "charset" "utf-8"
  , simpleNodeElement "title" "Hello from Haskell!" 
  ]

app :: IO SSDomIO -> SessionStorage -> Application
app = appWithHeadNodes defaultHeadNodes

appWithHeadNodes :: [Node] -> IO SSDomIO -> SessionStorage -> Application
appWithHeadNodes headNodes createNetwork sessionStorage = 
  alternatives 
    [ pathEnd $ method GET getMainDocument
    , path jsDriverName $ pathEnd $ getJsDriver
    , path "events" $ pathEnd $ alternatives
      [ method POST $ withSessionId processNewEvents
      , method GET $ withSessionId getDelta
      ]
    ]

  where

    getJsDriver = complete $ responseLBS status200 [] (LBS.fromStrict jsDriver)

    getMainDocument = maybeSingleParameter "sid" $ \maybeSessionId -> completeIO $ do
      timeoutVar <- registerDelay firstLoadTimeout
      sessionId <- maybe createNewUiSession return maybeSessionId
      atomically $ do
        maybeSnapshot <- getNewSnapshot sessionStorage sessionId
        case maybeSnapshot of
          Nothing -> return sessionNotFoundResponse
          Just (out, snapshotId) -> do
            isTimedOut <- readTVar timeoutVar
            check (not $ null out || isTimedOut)
            return $ completePage sessionId snapshotId out

    processNewEvents sessionId = entityJson $ \eventPayload -> completeIO $ do
      result <- sendEventIO sessionStorage sessionId eventPayload
      return $ if result
        then responseLBS status200 [] "ok"
        else sessionNotFoundResponse

    getDelta sessionId = singleParameter "snapshot" $ \snapshotId -> completeIO $ do
      timeoutVar <- registerDelay eventsDelay
      atomically $ do
        maybeSessionAndSnapshot <- lookupSnapshot sessionStorage sessionId snapshotId
        case maybeSessionAndSnapshot of
          Nothing -> return sessionNotFoundResponse
          Just (session, maybeOldState) -> do
            let oldState = fromMaybe [] maybeOldState
            newOut <- snd $ getSSDomIO session
            isTimeout <- readTVar timeoutVar
            check $ newOut /= oldState || isTimeout
            maybeNewSnapshot <- getNewSnapshot sessionStorage sessionId
            return $ case maybeNewSnapshot of
              Nothing -> sessionNotFoundResponse
              Just (newState, newSnapshotId) -> 
                responseLBS status200 [] (encode (newSnapshotId, delta $ diff oldState newState))

    sessionNotFoundResponse = responseLBS status400 [] "No session found"

    completePage sessionId snapshotId out =
      responseLBS status200 [] (renderHtml $ toMarkup (mainPage headNodes sessionId snapshotId out))

    createNewUiSession = createNetwork >>= newSession sessionStorage

    withSessionId = singleParameter "sid"

mainPage :: [Node] -> Text -> Int -> [Node] -> Document
mainPage headNodes sid snapshot inner =
  Document 
    (Prologue [] (Just (Doctype "html" Nothing)) [])
    (Element "html" (Map.singleton "style" "height: 100%;")
      [ headElement
      , bodyElement
      ]
    )
    []
  where
    headElement = 
      NodeElement $ Element "head" Map.empty 
        (headNodes ++ 
          [NodeElement $ Element "script" (Map.fromList [("type", "text/javascript"), ("src", jsDriverName)]) []])
    bodyElement =
      NodeElement $ Element "body" (bodyAttributes sid snapshot) inner
    
bodyAttributes sid snapshot = Map.fromList
  [ ("id", "purescheme-ssd-main-app")
  , ("data-purescheme-ssd-sid", sid)
  , ("data-purescheme-ssd-snapshot", T.pack $ show snapshot)
  , ("style", "display: flex")
  ]