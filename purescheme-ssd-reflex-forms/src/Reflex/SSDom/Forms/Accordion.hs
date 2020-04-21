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
{-# language TemplateHaskell #-}
module Reflex.SSDom.Forms.Accordion
  ( accordion
  , accordionPanel
  )
where

import Reflex.SSDom
import Text.XML.Simple

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Reflex
import Text.XML 

data AccordionPanelConfig t
  = AccordionPanelConfig
  { _accordionPanelConfig_autofocus :: Dynamic t Bool
  , _accordionPanelConfig_disabled :: Dynamic t Bool
  , _accordionPanelConfig_visible :: Dynamic t Bool 
  }

instance Reflex t => Default (AccordionPanelConfig t) where
  def = 
    AccordionPanelConfig
    { _accordionPanelConfig_autofocus = constDyn False
    , _accordionPanelConfig_disabled = constDyn False
    , _accordionPanelConfig_visible = constDyn True
    }

-- TODO: Should we force only accordionPanels inside?
accordion :: (SSDWidgetMonad t m) => m a -> m a
accordion inner = do
  (result, inner) <- runInner inner
  let
    htmlDyn = do
      nInner <- inner
      return [NodeElement $ vaadinAccordion nInner ]
  tellNodes htmlDyn
  return result

accordionPanel 
  :: (SSDWidgetMonad t m)
  => AccordionPanelConfig t 
  -> m ()
  -> m a
  -> m a
accordionPanel config summary inner = do
  (_, summaryNodes) <- runInner summary
  (result, innerNodes) <- runInner inner
  let 
    htmlDyn = do
      nVisible <- _accordionPanelConfig_visible config
      nDisabled <- _accordionPanelConfig_disabled config
      nAutofocus <- _accordionPanelConfig_autofocus config
      nSummaryNodes <- summaryNodes
      nInnerNodes <- innerNodes
      if nVisible
        then return 
          [ NodeElement $ vaadinAccordionPanel nSummaryNodes nInnerNodes
            ! booleanAttribute "disabled" nDisabled
            ! booleanAttribute "autofocus" nAutofocus
          ]
        else return mempty
  tellNodes htmlDyn
  return result

vaadinAccordion :: [Node] -> Element
vaadinAccordion = Element "vaadin-accordion" Map.empty

vaadinAccordionPanel :: [Node] -> [Node] -> Element
vaadinAccordionPanel summary rest = Element "vaadin-accordion-panel" Map.empty innerElements
  where 
    innerElements = 
      [ NodeElement $ Element "div" (Map.fromList [("slot", "summary")]) summary
      , NodeElement $ Element "div" Map.empty rest
      ]

