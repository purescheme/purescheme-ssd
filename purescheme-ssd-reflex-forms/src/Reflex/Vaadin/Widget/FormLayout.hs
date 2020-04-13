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
module Reflex.Vaadin.Widget.FormLayout where

import Reflex.SSDom

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadFix)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Reflex
import Text.XML

data FormLayoutConfig t
  = FormLayoutConfig
  { _formLayoutConfig_visible :: Dynamic t Bool
  }

instance Reflex t => Default (FormLayoutConfig t) where
  def = 
    FormLayoutConfig
    { _formLayoutConfig_visible = constDyn True
    }

formLayout :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m, MonadFix m) => 
  FormLayoutConfig t -> m a -> m a
formLayout config inner = do
  (result, innerHtml) <- runInner inner
  let 
    htmlDyn = do
      nVisible <- _formLayoutConfig_visible config
      nInner <- innerHtml
      if nVisible
        then return 
          [ NodeElement $ vaadinFormLayout nInner
          ]
        else return mempty
  tellNodes htmlDyn
  return result


vaadinFormLayout :: [Node] -> Element
vaadinFormLayout = Element "vaadin-form-layout" Map.empty

