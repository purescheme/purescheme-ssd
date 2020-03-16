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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.SSDom
  ( SSDWidget
  , SSDWidgetContext
  , SSDConstraints
  , SSDWidgetMonad(..)
  , FrontendEvent(..)
  , nextId
  , runSSDWidget
  , mainSSDWidget
  , filterEvents
  )
where

import Purescheme.SSDom

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, takeTMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Lens ((<&>))
import Control.Monad (void, forever, unless, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (ReaderT, MonadReader, MonadTrans, ask, lift, runReaderT)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.STM (atomically)
import Data.Aeson (Value)
import Data.Dependent.Sum (DSum(..), (==>))
import Data.Foldable (for_, traverse_)
import Data.Functor.Identity (Identity)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Traversable (for)
import Reflex
import Reflex.Class ()
import Reflex.Host.Class
import Text.XML (Node)

data SSDWidgetContext t
  = SSDWidgetContext
  { _widgetContext_widgetIdsCounter :: IORef Int
  , _widgetContext_frontendEvent :: Event t FrontendEvent
  }

newtype SSDWidget t m a
  = SSDWidget
  { unSSDWidget :: DynamicWriterT t [Node] (ReaderT (SSDWidgetContext t) m) a -- :: DynamicWriterT t [Node] (ReaderT (SSDWidgetContext t) m) a-- (PostBuildT t (TriggerEventT t (PerformEventT t m)))) a -- :: PostBuildT t (TriggerEventT t (PerformEventT t m)) a
  } deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadSample t
    , MonadHold t
    , MonadFix
    , NotReady t
    , PostBuild t
    , TriggerEvent t
    , MonadReflexCreateTrigger t
    , MonadIO
    )

deriving instance (Reflex t, Functor m, Monad m) => DynamicWriter t [Node] (SSDWidget t m)
deriving instance (Monad m, Functor m) => MonadReader (SSDWidgetContext t) (SSDWidget t m)
deriving instance PerformEvent t m => PerformEvent t (SSDWidget t m)

instance (Adjustable t m, MonadHold t m, Reflex t, MonadFix m) => Adjustable t (SSDWidget t m) where
  runWithReplace a0 a' = SSDWidget $ runWithReplace (unSSDWidget a0) $ fmap unSSDWidget a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = SSDWidget $
    traverseIntMapWithKeyWithAdjust (\k v -> unSSDWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = SSDWidget $ 
    traverseDMapWithKeyWithAdjust (\k v -> unSSDWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = SSDWidget $ 
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unSSDWidget (f k v)) dm0 dm'

instance MonadTrans (SSDWidget t) where
  lift f = SSDWidget $ lift $ lift f

type SSDConstraints t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , Adjustable t m
  )

class (Monad m, Reflex t, NotReady t m, Adjustable t m, MonadHold t m, MonadFix m, MonadIO m, PostBuild t m) => SSDWidgetMonad t m | m -> t where
  tellNodes :: Dynamic t [Node] -> m ()
  runInner :: m a -> m (a, Dynamic t [Node])
  nextId :: m (Int, Event t (Text, Value))

instance (Monad m, Reflex t, NotReady t m, Adjustable t m, MonadHold t m, MonadFix m, MonadIO m, PostBuild t m) => SSDWidgetMonad t (SSDWidget t m) where
  tellNodes dynNodes = SSDWidget $ tellDyn dynNodes
  runInner (SSDWidget actual) = do
    context <- ask
    (result, inner) <- lift $ runReaderT (runDynamicWriterT actual) context
    return (result, inner)
  nextId = do
    SSDWidgetContext idRef inputEvent <- ask
    newId <- liftIO $ atomicModifyIORef idRef (\i -> (i + 1, i + 1))
    let componentEvents = fmap (\(FrontendEvent a b c) -> (a, b, c)) inputEvent
    let componentEvents2 = ffilter (\(componentId, _, _) -> componentId == newId) componentEvents
    let componentEvents3 = fmap (\(_, a, b) -> (a, b)) componentEvents2
    return (newId, componentEvents3)

mainSSDWidget :: (forall t m. SSDConstraints t m => SSDWidget t m (Event t ())) -> IO SSDomIO
mainSSDWidget network = do
  inputTMVar <- newEmptyTMVarIO
  outputTVar <- newTVarIO mempty
  void $ forkIO $ basicHostWithQuit $ do
    (linesIn, input) <- newTriggerEvent
    pb <- getPostBuild
    void $ liftIO $ forkIO $ forever (atomically (takeTMVar inputTMVar) >>= input)
    (quitEvent, htmlDyn) <- runSSDWidget linesIn network
    outputEvents1 <- debounce 0.001 $ leftmost [tagPromptlyDyn htmlDyn pb, updated htmlDyn]
    output outputTVar outputEvents1
    return quitEvent
  return (putTMVar inputTMVar, readTVar outputTVar)

runSSDWidget ::  SSDConstraints t m => Event t FrontendEvent -> SSDWidget t m (Event t ()) -> m (Event t (), Dynamic t [Node])
runSSDWidget input widget = do
    idRef <- liftIO $ newIORef 0
    runReaderT ( runDynamicWriterT (unSSDWidget widget)) (SSDWidgetContext idRef input)

output :: (PerformEvent t m, MonadIO (Performable m), Reflex t) => TVar [Node] -> Event t [Node] -> m ()
output varState ev =
  performEvent_ (ffor ev $ \s -> liftIO $ atomically $ writeTVar varState s)

-- Copied from Reflex.Basic.Host
basicHostWithQuit
  :: (forall t m. SSDConstraints t m => m (Event t ()))
  -> IO ()
basicHostWithQuit guest =
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Unpack the guest, get the quit event, the result of building the
    -- network, and a function to kick off each frame.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    triggerEventChan <- liftIO newChan
    rHasQuit <- newRef False -- When to shut down
    (eQuit, FireCommand fire) <- hostPerformEventT
      . flip runTriggerEventT triggerEventChan
      . flip runPostBuildT postBuild
      $ guest

    hQuit <- subscribeEvent eQuit
    let
      runFrameAndCheckQuit firings = do
        lmQuit <- fire firings $ readEvent hQuit >>= sequenceA
        when (any isJust lmQuit) $ writeRef rHasQuit True

    -- If anyone is listening to PostBuild, fire it
    readRef postBuildTriggerRef
      >>= traverse_ (\t -> runFrameAndCheckQuit [t ==> ()])

    let
      loop = do
        hasQuit <- readRef rHasQuit
        unless hasQuit $ do
          eventsAndTriggers <- liftIO $ readChan triggerEventChan

          let
            prepareFiring
              :: (MonadRef m, Ref m ~ Ref IO)
              => DSum (EventTriggerRef t) TriggerInvocation
              -> m (Maybe (DSum (EventTrigger t) Identity))
            prepareFiring (EventTriggerRef er :=> TriggerInvocation x _)
              = readRef er <&> fmap (==> x)

          catMaybes <$> for eventsAndTriggers prepareFiring
            >>= runFrameAndCheckQuit

          -- Fire callbacks for each event we triggered this frame
          liftIO . for_ eventsAndTriggers $
            \(_ :=> TriggerInvocation _ cb) -> cb
          loop
    loop

filterEvents :: (Reflex t) => Text -> Event t (Text, Value) -> Event t Value
filterEvents eventName evs = snd <$> ffilter (\ev -> fst ev == eventName) evs
