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
module Network.Wai.SSDom.Session
  ( SessionStorage
  , Session
  , newSession
  , getNewSnapshot
  , emptySessionStorage
  , sendEventIO
  , getSessionById
  , lookupSnapshot
  , getSSDomIO
  )
where

import Purescheme.SSDom

import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar, newTVarIO)
import Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomIO)
import Text.XML (Node)

data Session
  = Session
  { _session_SSDomIO :: SSDomIO
  , _session_lastSnapshot :: Int
  , _session_lastStates :: LRU Int [Node]
  }

newtype SessionStorage = SessionStorage (TVar (Map Text Session))

emptySessionStorage :: IO SessionStorage
emptySessionStorage = SessionStorage <$> newTVarIO Map.empty

newSession :: SessionStorage -> SSDomIO -> IO Text
newSession (SessionStorage tvarMap) ssDomIO = do
  sessionId <- generateNewSessionId
  atomically $ modifyTVar' tvarMap $ Map.insert sessionId $ createSession ssDomIO
  return sessionId
  where
    createSession io = Session io 0 (newLRU (Just 5))

getNewSnapshot :: SessionStorage -> Text -> STM (Maybe ([Node], Int))
getNewSnapshot sessionStorage@(SessionStorage tvarMap) sessionId = do
  maybeSession <- getSessionById sessionStorage sessionId
  case maybeSession of
    Nothing -> return Nothing
    Just session -> Just <$> getNewSnapshot' session

  where

    getNewSnapshot' :: Session -> STM ([Node], Int)
    getNewSnapshot' session = do
      out <- snd $ _session_SSDomIO session
      let nextSnapshot = _session_lastSnapshot session + 1
      let newLruCache = LRU.insert nextSnapshot out (_session_lastStates session)
      modifyTVar' tvarMap $ Map.insert sessionId $ session {_session_lastSnapshot = nextSnapshot, _session_lastStates = newLruCache }
      return (out, nextSnapshot)

lookupSnapshot :: SessionStorage -> Text -> Int -> STM (Maybe (Session, Maybe [Node]))
lookupSnapshot (SessionStorage tvarMap) sessionId snapshotId = do
  maybeSession <- Map.lookup sessionId <$> readTVar tvarMap
  case maybeSession of
    Nothing -> return Nothing
    Just session -> do
      let (newLru, maybeSnapshot) = LRU.lookup snapshotId (_session_lastStates session)
      modifyTVar' tvarMap $ Map.insert sessionId (session {_session_lastStates = newLru}) 
      return $ Just (session, maybeSnapshot)

sendEventIO :: SessionStorage -> Text -> FrontendEvent -> IO Bool
sendEventIO sessionStorage sessionId event = atomically $ do
  maybeSession <- getSessionById sessionStorage sessionId
  case maybeSession of
    Nothing -> return False
    Just session -> (fst $ _session_SSDomIO session) event >> return True

getSessionById :: SessionStorage -> Text -> STM (Maybe Session)
getSessionById (SessionStorage tvarMap) sessionId = readTVar tvarMap <&> Map.lookup sessionId

generateNewSessionId :: IO Text
generateNewSessionId = fmap (T.pack . show . abs) (randomIO :: IO Int)

getSSDomIO :: Session -> SSDomIO
getSSDomIO = _session_SSDomIO
