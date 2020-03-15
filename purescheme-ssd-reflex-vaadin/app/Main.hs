{-# language FlexibleContexts, TypeFamilies #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Paths_purescheme_ssd_reflex_vaadin

import Network.Wai.SSDom
import Reflex.SSDom
import Reflex.Vaadin.Widget

import Control.Monad (void)
import Data.Default (Default(..))
import Network.Wai.Handler.Warp (run)
import Reflex

{-
import Network.Wai.SSDom


import Reflex hiding (Request, Response)
import Reflex.Network

import Control.Applicative (liftA2)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.STM (STM, atomically, check)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue)
import Control.Monad.Reader
import Data.Aeson (Value, toJSON, decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef, atomicWriteIORef)
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types.Status (Status, status200, status303, status403)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Network.Wai.Routing.Purescheme.Core
import Network.Wai.Routing.Purescheme.Core.Internal
import System.Random (randomIO)
import Text.XML
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze (toMarkup)

import Text.XML.Delta
-}
main :: IO ()
main = do
  dataDir <- getDataDir
  sessionStorage <- emptySessionStorage
  run 9090 $ app (dataDir ++ "/js/dist") (mainSSDWidget exampleUi) sessionStorage

exampleUi :: (SSDConstraints t m) => SSDWidget t m (Event t ())
exampleUi = do
  jsScript "vaadin.js"
  verticalLayout def{_orderedLayoutConfig_margin = pure True, _orderedLayoutConfig_padding = pure True, _orderedLayoutConfig_spacing = pure True} $ do
    horizontalLayout def{_orderedLayoutConfig_margin = pure True, _orderedLayoutConfig_padding = pure True, _orderedLayoutConfig_spacing = pure True} $ mdo
      button1 <- button def{_buttonConfig_label = "Click me"}
      let checkboxState = tag (current $ _checkbox_checked cb1) (_button_click button1)
      let setChecked = fmap not checkboxState
      cb1 <- checkbox def{ _checkboxConfig_label = "Checbox 1", _checkboxConfig_setChecked = setChecked } False
      horizontalLayout def{_orderedLayoutConfig_visible =  _checkbox_checked cb1} $ mdo
        cb2 <- checkbox def{ _checkboxConfig_label = "Checkbox 2" } False
        button2 <- button def{ _buttonConfig_visible = _checkbox_checked cb2, _buttonConfig_label = "Clieck me" }
        void $ textField def{_textFieldConfig_setValue = fmap (const "") $ _button_click button2,  _textFieldConfig_visible = _checkbox_checked cb2, _textFieldConfig_label = "The Field"} "Hello"
        return ()
    formLayout def $ do
      void $ textField def{ _textFieldConfig_label = "First Name" } ""
      void $ textField def{ _textFieldConfig_label = "Last Name" } ""
      void $ textField def{ _textFieldConfig_label = "Email" } ""
      void $ textField def{ _textFieldConfig_label = "Birthday" } ""
      void $ textField def{ _textFieldConfig_label = "Example" } ""
  return never -- TODO How to do the quit
