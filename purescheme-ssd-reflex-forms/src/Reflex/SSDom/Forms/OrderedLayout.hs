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
module Reflex.SSDom.Forms.OrderedLayout where

import Reflex.SSDom
import Text.XML.Simple

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadFix)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Reflex
import qualified Data.Text as T
import Text.XML

data OrderedLayoutConfig t
  = OrderedLayoutConfig
  { _orderedLayoutConfig_margin :: Dynamic t Bool
  , _orderedLayoutConfig_padding :: Dynamic t Bool
  , _orderedLayoutConfig_spacing :: Dynamic t Bool
  , _orderedLayoutConfig_visible :: Dynamic t Bool
  }

instance Reflex t => Default (OrderedLayoutConfig t) where
  def = 
    OrderedLayoutConfig
    { _orderedLayoutConfig_margin = constDyn False
    , _orderedLayoutConfig_padding = constDyn False
    , _orderedLayoutConfig_spacing = constDyn False
    , _orderedLayoutConfig_visible = constDyn True
    }

-- verticalLayout :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m, MonadFix m) => 
--   OrderedLayoutConfig t -> DynamicWriterT t [Node] m a -> m a
verticalLayout :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m, MonadFix m) => 
  OrderedLayoutConfig t -> m a -> m a
verticalLayout = orderedLayout vaadinVerticalLayout

-- horizontalLayout :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m, MonadFix m) => 
--   OrderedLayoutConfig t -> DynamicWriterT t [Node] m a -> m a
horizontalLayout :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m, MonadFix m) => 
  OrderedLayoutConfig t -> m a -> m a
horizontalLayout = orderedLayout vaadinHorizontalLayout

-- orderedLayout :: (SSDWidgetMonad t m, MonadFix m) => 
--   ([Node] -> Element) -> OrderedLayoutConfig t -> DynamicWriterT t [Node] m a -> m a
orderedLayout :: (SSDWidgetMonad t m, MonadFix m) => 
  ([Node] -> Element) -> OrderedLayoutConfig t -> m a -> m a
orderedLayout createElement config inner = do
  (result, innerHtml) <- runInner inner
  let 
    htmlDyn = do
      nMargin <- _orderedLayoutConfig_margin config
      nPadding <- _orderedLayoutConfig_padding config
      nSpacing <- _orderedLayoutConfig_spacing config
      nVisible <- _orderedLayoutConfig_visible config
      nInner <- innerHtml
      let theme = T.intercalate " " $ catMaybes [toMaybe nMargin "margin", toMaybe nPadding "padding", toMaybe nSpacing "spacing"]
      if nVisible
        then return 
          [ NodeElement $ createElement nInner
          ! attribute "theme" theme
          ]
        else return mempty
  tellNodes htmlDyn
  return result
  where
    toMaybe :: Bool -> a -> Maybe a
    toMaybe True a = Just a
    toMaybe False _ = Nothing

vaadinHorizontalLayout :: [Node] -> Element
vaadinHorizontalLayout = Element "vaadin-horizontal-layout" Map.empty

vaadinVerticalLayout :: [Node] -> Element
vaadinVerticalLayout = Element "vaadin-vertical-layout" Map.empty
