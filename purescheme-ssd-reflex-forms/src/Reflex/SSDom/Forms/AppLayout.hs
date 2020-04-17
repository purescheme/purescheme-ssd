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
{-# language FlexibleContexts, TypeFamilies #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Reflex.SSDom.Forms.AppLayout where

import Reflex.SSDom
import Text.XML.Simple

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadFix)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Text.XML

data AppLayoutConfig t
  = AppLayoutConfig
  { _appLayoutConfig_style :: Dynamic t [(Text, Text)]
  }

instance Reflex t => Default (AppLayoutConfig t) where
  def = 
    AppLayoutConfig
    { _appLayoutConfig_style = constDyn []
    }

appLayout :: (SSDWidgetMonad t m) => AppLayoutConfig t -> m a -> m a
appLayout config inner = do
  (result, innerHtml) <- runInner inner
  let 
    htmlDyn = do
      nInnerHtml <- innerHtml
      nStyles <- _appLayoutConfig_style config
      return
        [ NodeElement $ Element "vaadin-app-layout" Map.empty nInnerHtml
          ! attribute "style" (T.intercalate ";" (fmap (\(k,v) -> T.intercalate ":" [k, v]) nStyles))
        ]
  tellNodes htmlDyn
  return result

drawerToggle :: (SSDWidgetMonad t m) => m ()
drawerToggle = do
  tellNodes $ pure [leafNode "vaadin-drawer-toggle"]
  return ()

navbar :: (SSDWidgetMonad t m) => m a -> m a
navbar = withSlot "navbar"

drawer :: (SSDWidgetMonad t m) => m a -> m a
drawer = withSlot "drawer"

withSlot :: (SSDWidgetMonad t m) => Text -> m a -> m a
withSlot slotName inner = do
  (result, innerHtml) <- runInner inner
  let
    htmlDyn = do
      nInnerHtml <- innerHtml
      return $ withSlot' slotName nInnerHtml
  tellNodes htmlDyn
  return result

withSlot' :: Text -> [Node] -> [Node]
withSlot' slotName = go
  where
    go (x:xs) = case x of 
      NodeElement el -> (NodeElement $ el ! attribute "slot" slotName):(go xs)
      otherCase -> otherCase:(go xs) -- Do not support other case, just won't be added to the slot
    go [] = []
    