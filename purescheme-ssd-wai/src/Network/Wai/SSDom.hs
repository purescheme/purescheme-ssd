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
  , module Network.Wai.SSDom.Session
  )
where

import Network.Wai.SSDom.Session
import Purescheme.SSDom
import Text.XML.Simple
import Text.XML.Delta

import Control.Monad.STM (atomically, check)
import Control.Concurrent.STM.TVar (registerDelay, readTVar)
import Data.Aeson (encode)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status (status200, status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Routing.Purescheme.Core
import Text.Blaze (toMarkup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.XML

jsDriverName :: String
jsDriverName = "ssd-driver.js"

eventsDelay :: Int
eventsDelay = 60 * 1_000_000

firstLoadTimeout :: Int
firstLoadTimeout = 400_000

-- TODO: Use STM for session and withSession instead of withSessionId and store it all the time
app :: FilePath -> IO SSDomIO -> SessionStorage -> Application
app jsBasePath createNetwork sessionStorage = 
  staticPolicy (addBase jsBasePath) $ alternatives 
    [ pathEnd $ method GET getMainDocument
    , path "events" $ pathEnd $ alternatives
      [ method POST $ withSessionId processNewEvents
      , method GET $ withSessionId getDelta
      ]
    ]
    where

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
        responseLBS status200 [] (renderHtml $ toMarkup (mainPage sessionId snapshotId out))

      createNewUiSession = createNetwork >>= newSession sessionStorage

      withSessionId = singleParameter "sid"

mainPage :: Text -> Int -> [Node] -> Document
mainPage sid snapshot inner =
    Document 
      (Prologue [] (Just (Doctype "html" Nothing)) [])
      (Element "html" Map.empty 
        [ NodeElement $ Element "head" Map.empty 
          [ NodeElement $ leafElement "meta" ! attribute "charset" "utf-8"
          , simpleNodeElement "title" "Hello from Haskell!" 
          ]
        , NodeElement $ Element "body" (Map.singleton "data-reflex-vaadin-sid" sid)
          [ NodeElement $ Element "script" (Map.fromList [("type", "text/javascript"), ("src", T.pack jsDriverName)]) []
          , NodeElement $ divVaadinApp snapshot inner
          ]
        ]
      )
      []

divVaadinApp :: Int -> [Node] -> Element
divVaadinApp snapshot inner = 
  Element "div" Map.empty inner 
  ! attribute "id" "reflex-vaadin-main-app"
  ! attribute "data-reflex-vaadin-snapshot" (T.pack $ show snapshot)

