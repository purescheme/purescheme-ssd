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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Vaadin.Widget.TextField where

import Reflex.SSDom
import Reflex.Vaadin.Widget.Internal
import Text.XML.Simple

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson 
import Data.Default (Default(..))
import qualified Data.Map as Map
import Reflex
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML

data TextFieldConfig t
  = TextFieldConfig
  { _textFieldConfig_autofocus :: Dynamic t Bool
  , _textFieldConfig_label :: Dynamic t Text
  , _textFieldConfig_visible :: Dynamic t Bool
  , _textFieldConfig_disabled :: Dynamic t Bool
  , _textFieldConfig_setValue :: Event t Text
  }

instance Reflex t => Default (TextFieldConfig t) where
  def = 
    TextFieldConfig
    { _textFieldConfig_autofocus = constDyn False
    , _textFieldConfig_label = constDyn ""
    , _textFieldConfig_disabled = constDyn False
    , _textFieldConfig_visible = constDyn True
    , _textFieldConfig_setValue = never
    }

data TextField t
  = TextField
  { _textField_value :: Dynamic t Text
  , _textField_change :: Event t ()
  }

textField :: (MonadHold t m, SSDWidgetMonad t m, MonadIO m) => 
  TextFieldConfig t -> Text -> m (TextField t)
textField config initialState = do
  (componentId, evs) <- nextId
  let inputEv = mapInputValue $ filterEvents "input" evs
  let changeEv = void $ filterEvents "change" evs
  valueDyn <- holdDyn initialState $ leftmost [_textFieldConfig_setValue config, fmap snd inputEv]
  snapshotDyn <- holdDyn 0 (fmap fst inputEv)
  let 
    htmlDyn = do
      nLabel <- _textFieldConfig_label config
      nAutofocus <- _textFieldConfig_autofocus config
      nDisabled <- _textFieldConfig_disabled config
      nVisible <- _textFieldConfig_visible config
      nSnapshot <- snapshotDyn
      nValue <- valueDyn
      if nVisible
        then return
          [ NodeElement $ vaadinTextField mempty
          ! attribute "id" (T.pack $ show componentId)
          ! booleanAttribute "autofocus" nAutofocus
          ! booleanAttribute "disabled" nDisabled
          ! attribute "value" nValue
          ! attribute "label" nLabel
          ! attribute "data-frp-event-value" (T.intercalate "|" [ "input", T.pack $ show nSnapshot, nValue])
          ! attribute "data-frp-snapshot-input" (T.pack $ show nSnapshot)
          ! ssDomEventsAttribute events
          ]
        else return mempty
  tellNodes htmlDyn
  return $ TextField valueDyn changeEv
  where
    events = ["change", "input"]

    mapInputValue :: (Reflex t) => Event t Value -> Event t (Int, Text)
    mapInputValue = fmapMaybe extractInputValue
    extractInputValue eventPayload = aesonResultToMaybe $ do
      v1 <- fromJSON eventPayload
      value <- fromJSON $ v1 Map.! ("value" :: Text)
      snapshot <- fromJSON $ v1 Map.! ("snapshot" :: Text)
      return (snapshot, value)

vaadinTextField :: [Node] -> Element
vaadinTextField = Element "vaadin-text-field" Map.empty
