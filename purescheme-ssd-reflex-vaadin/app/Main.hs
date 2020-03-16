{-# language FlexibleContexts, TypeFamilies #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Paths_purescheme_ssd_reflex_vaadin

-- import Development.Placeholders

import Prelude hiding (span)

import Network.Wai.SSDom
import Reflex.SSDom
import Reflex.Vaadin.Widget

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import Data.Default (Default(..))
import Network.Wai.Handler.Warp (run)
import Reflex
import Reflex.Network
import Text.XML (Node)

main :: IO ()
main = do
  dataDir <- getDataDir
  sessionStorage <- emptySessionStorage
  run 9090 $ app (dataDir ++ "/js/dist") (mainSSDWidget demoUI) sessionStorage

-- TODO This needs to be menu :: (SSDWidgetMonad t m) =>  m (Dynamic t (m ()))
-- demoUI :: (SSDConstraints t m) => SSDWidget t m (Event t ())
demoUI :: (SSDWidgetMonad t m) =>  m (Event t ())
demoUI = do
  jsScript "vaadin.js"
  horizontalLayout def $ do
    demoContent <- menu
    networkView $ demoContent
    return never

menu :: (SSDWidgetMonad t m) =>  m (Dynamic t (m ()))
menu = 
  accordion $ do
    result1 <- accordionPanel def (span "Basic features") $ basicFeatures
    result2 <- accordionPanel def (span "Data input") $ dataInput
    holdDyn initialGui $ leftmost 
      [ updated result1
      , updated result2
      ]

basicFeatures :: (SSDWidgetMonad t m) => m (Dynamic t (m ()))
basicFeatures = do
      verticalLayout def $ do
        iconButton <- button def{_buttonConfig_label = "Icon"}
        tooltipButton <- button def{_buttonConfig_label = "Tooltip"}
        holdDyn initialGui $ leftmost 
          [ fmap (const iconGui) $ _button_click iconButton 
          , fmap (const tooltipGui) $ _button_click tooltipButton
          ]

dataInput :: (SSDWidgetMonad t m) => m (Dynamic t (m ()))
dataInput = do
    accordion $ do
      accordionPanel def (span "Textual") $ do 
        verticalLayout def $ do
          textButton <- button def{_buttonConfig_label = "Text field"}
          holdDyn initialGui $ leftmost
            [ fmap (const textGui) $ _button_click textButton
            ]

textGui :: (SSDWidgetMonad t m) => m ()
textGui = span "Text Gui"

initialGui :: (SSDWidgetMonad t m) => m ()
initialGui = span "Initial Gui"

iconGui :: (SSDWidgetMonad t m) => m ()
iconGui = horizontalLayout def $ do
  ironIcon "vaadin:vaadin-h"  

tooltipGui :: (SSDWidgetMonad t m) => m ()
tooltipGui = span "Tooltip GUI!"

exampleUi :: (SSDWidgetMonad t m) => m (Event t ())
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
