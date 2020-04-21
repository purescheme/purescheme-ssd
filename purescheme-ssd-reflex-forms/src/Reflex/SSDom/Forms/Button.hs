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
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Text.XML 


data ButtonType 
  = ButtonTypePrimary 
  | ButtonTypeSecondary 
  | ButtonTypeTertiary 
  | ButtonTypeTertiaryInline

data ButtonColor
  = ButtonColorDefault
  | ButtonColorContrast
  | ButtonColorSuccess
  | ButtonColorError

data ButtonSize
  = ButtonSizeLarge
  | ButtonSizeNormal
  | ButtonSizeSmall

data ButtonConfig t
  = ButtonConfig
  { _buttonConfig_autofocus :: Dynamic t Bool
  , _buttonConfig_label :: Dynamic t Text
  , _buttonConfig_visible :: Dynamic t Bool
  , _buttonConfig_disabled :: Dynamic t Bool
  , _buttonConfig_type :: Dynamic t ButtonType
  , _buttonConfig_color :: Dynamic t ButtonColor
  , _buttonConfig_size :: Dynamic t ButtonSize
  }

instance Reflex t => Default (ButtonConfig t) where
  def = 
    ButtonConfig
    { _buttonConfig_autofocus = constDyn False
    , _buttonConfig_label = constDyn ""
    , _buttonConfig_disabled = constDyn False
    , _buttonConfig_visible = constDyn True
    , _buttonConfig_type = constDyn ButtonTypeSecondary
    , _buttonConfig_color = constDyn ButtonColorDefault
    , _buttonConfig_size = constDyn ButtonSizeNormal
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

  let typeTheme ButtonTypePrimary = Just "primary"
      typeTheme ButtonTypeSecondary = Nothing
      typeTheme ButtonTypeTertiary = Just "tertiary"
      typeTheme ButtonTypeTertiaryInline = Just "tertiary-inline"
  let colorTheme ButtonColorDefault = Nothing
      colorTheme ButtonColorContrast = Just "contrast"
      colorTheme ButtonColorSuccess = Just "success"
      colorTheme ButtonColorError = Just "error"
  let sizeTheme ButtonSizeLarge = Just "large"
      sizeTheme ButtonSizeNormal = Nothing
      sizeTheme ButtonSizeSmall = Nothing

  let buildTheme t c s = T.intercalate " " $ catMaybes [typeTheme t, colorTheme c, sizeTheme s]
  let 
    htmlDyn = do
      nLabel <- _buttonConfig_label config
      nAutofocus <- _buttonConfig_autofocus config
      nDisabled <- _buttonConfig_disabled config
      nVisible <- _buttonConfig_visible config
      nType <- _buttonConfig_type config
      nColor <- _buttonConfig_color config
      nSize <- _buttonConfig_size config
      if nVisible
        then return
          [ NodeElement $ vaadinButtonElement [NodeContent nLabel]
          ! attribute "id" (T.pack $ show componentId)
          ! booleanAttribute "autofocus" nAutofocus
          ! booleanAttribute "disabled" nDisabled
          ! attribute "theme" (buildTheme nType nColor nSize)
          ! ssDomEventsAttribute ["click"]
          ]
        else return mempty
  tellNodes htmlDyn
  return $ Button clickEv

vaadinButtonElement :: [Node] -> Element
vaadinButtonElement = Element "vaadin-button" Map.empty

