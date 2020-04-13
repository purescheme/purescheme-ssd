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
module Reflex.SSDom.Forms.Checkbox where

import Reflex.SSDom
import Reflex.SSDom.Forms.Internal
import Text.XML.Simple

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, fromJSON)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Reflex
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML

data CheckboxConfig t
  = CheckboxConfig
  { _checkboxConfig_autofocus :: Dynamic t Bool
  , _checkboxConfig_label :: Dynamic t Text
  , _checkboxConfig_visible :: Dynamic t Bool
  , _checkboxConfig_disabled :: Dynamic t Bool
  , _checkboxConfig_indeterminate :: Dynamic t Bool
  , _checkboxConfig_setChecked :: Event t Bool
  }

instance Reflex t => Default (CheckboxConfig t) where
  def = 
    CheckboxConfig
    { _checkboxConfig_autofocus = constDyn False
    , _checkboxConfig_label = constDyn ""
    , _checkboxConfig_indeterminate = constDyn False
    , _checkboxConfig_disabled = constDyn False
    , _checkboxConfig_visible = constDyn True
    , _checkboxConfig_setChecked = never
    }

data Checkbox t
  = Checkbox
  { _checkbox_checked :: Dynamic t Bool
  , _checkbox_change :: Event t ()
  }

checkbox :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m) => 
  CheckboxConfig t -> Bool -> m (Checkbox t)
checkbox config initialState = do
  (componentId, evs) <- nextId
  let checkedChangedEv = mapCheckedChanged $ filterEvents "checked-changed" evs 
  let changeEv = void $ filterEvents "change" evs
  checkedDyn <- holdDyn initialState $ leftmost [checkedChangedEv, _checkboxConfig_setChecked config]
  let 
    htmlDyn = do
      nLabel <- _checkboxConfig_label config
      nAutofocus <- _checkboxConfig_autofocus config
      nIndeterminate <- _checkboxConfig_indeterminate config
      nDisabled <- _checkboxConfig_disabled config
      nVisible <- _checkboxConfig_visible config
      nChecked <- checkedDyn
      if nVisible
        then return
          [ NodeElement $ vaadinCheckboxElement [NodeContent nLabel]
          ! attribute "id" (T.pack $ show componentId)
          ! booleanAttribute "autofocus" nAutofocus
          ! booleanAttribute "checked" nChecked
          ! booleanAttribute "indeterminate" nIndeterminate
          ! booleanAttribute "disabled" nDisabled
          ! ssDomEventsAttribute ["change", "checked-changed"]
          ]
        else return mempty
        
  tellNodes htmlDyn
  return $ Checkbox checkedDyn changeEv
  where

    mapCheckedChanged :: (Reflex t) => Event t Value -> Event t Bool
    mapCheckedChanged = fmapMaybe extractCheckedChanged

    extractCheckedChanged :: Value -> Maybe Bool
    extractCheckedChanged value = aesonResultToMaybe $ do
      v1 <- fromJSON value
      v2 <- fromJSON $ v1 Map.! ("value" :: String)
      fromJSON v2

vaadinCheckboxElement :: [Node] -> Element
vaadinCheckboxElement = Element "vaadin-checkbox" Map.empty

