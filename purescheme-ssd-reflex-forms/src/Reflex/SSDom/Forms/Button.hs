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
module Reflex.SSDom.Forms.Button where

import Reflex.SSDom
import Text.XML.Simple

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Text.XML 

data ButtonConfig t
  = ButtonConfig
  { _buttonConfig_autofocus :: Dynamic t Bool
  , _buttonConfig_label :: Dynamic t Text
  , _buttonConfig_visible :: Dynamic t Bool
  , _buttonConfig_disabled :: Dynamic t Bool
  }

instance Reflex t => Default (ButtonConfig t) where
  def = 
    ButtonConfig
    { _buttonConfig_autofocus = constDyn False
    , _buttonConfig_label = constDyn ""
    , _buttonConfig_disabled = constDyn False
    , _buttonConfig_visible = constDyn True
    }

data Button t
  = Button
  { _button_click :: Event t ()
  }

button :: (SSDWidgetMonad t m, MonadIO m) => 
  ButtonConfig t -> m (Button t)
button config = do
  (componentId, evs) <- nextId
  let clickEv = void $ filterEvents "click" evs
  let 
    htmlDyn = do
      nLabel <- _buttonConfig_label config
      nAutofocus <- _buttonConfig_autofocus config
      nDisabled <- _buttonConfig_disabled config
      nVisible <- _buttonConfig_visible config
      if nVisible
        then return
          [ NodeElement $ vaadinButtonElement [NodeContent nLabel]
          ! attribute "id" (T.pack $ show componentId)
          ! booleanAttribute "autofocus" nAutofocus
          ! booleanAttribute "disabled" nDisabled
          ! ssDomEventsAttribute ["click"]
          ]
        else return mempty
  tellNodes htmlDyn
  return $ Button clickEv

vaadinButtonElement :: [Node] -> Element
vaadinButtonElement = Element "vaadin-button" Map.empty

