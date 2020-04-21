{-# language FlexibleContexts, TypeFamilies #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Paths_purescheme_ssd_reflex_forms

-- import Development.Placeholders

import Prelude hiding (span, div)

import Network.Wai.SSDom
import Network.Wai.SSDom.Forms
import Reflex.SSDom
import Reflex.SSDom.Basic
import Reflex.SSDom.Forms

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import Data.Default (Default(..))
import Data.Functor (($>))
import Data.Text (Text)
import Data.String.Interpolate.IsString (i)
import Network.Wai.Handler.Warp (run)
import Reflex
import Reflex.Network
import Text.XML (Node)
import Text.XML.Simple

main :: IO ()
main = do
  dataDir <- getDataDir
  sessionStorage <- emptySessionStorage
  run 9090 $ formsApp "Hello from Haskell!" (mainSSDWidget demoUI) sessionStorage

demoUI :: SSDWidgetMonad t m =>  m (Event t ())
demoUI = do
  appLayout (AppLayoutConfig (constDyn [("flex-grow", "1")])) $ do
    demoContent <- drawer menu
    navbar $ do
      drawerToggle
      constSimpleElement "h4" "Wellcome to purescheme forms demo!"
    networkView $ demoContent
    return never

menu :: (SSDWidgetMonad t m) =>  m (Dynamic t (m ()))
menu = 
  verticalLayout def{_orderedLayoutConfig_margin = constDyn True} $ 
    accordion $ do
      results <- sequence
        [ accordionPanel def (span "Form Input") $ formInput
        , accordionPanel def (span "Visualization") $ visualization
        ]
      holdDyn initialGui $ fmap demoLayout (leftmost results)

menuButton :: (SSDWidgetMonad t m) => Text -> m () -> m (Event t (m ()))
menuButton label ui = do
  but <- buildButton
  return $ _button_click but $> wrappedUi
  where
    buildButton = button def
      { _buttonConfig_label = constDyn label
      , _buttonConfig_type = constDyn ButtonTypeTertiaryInline
      }
    wrappedUi = verticalLayout def{_orderedLayoutConfig_margin = constDyn True,  _orderedLayoutConfig_style = constDyn [("flex-grow", "1")]} $ do
      constSimpleElement "h2" label
      ui

formInput :: (SSDWidgetMonad t m) => m (Event t (m ()))
formInput = do
  verticalLayout def $ do
    results <- sequence
      [ menuButton "Checkbox" checkboxGui
      , menuButton "Text field" textGui
      ]
    return $ leftmost results

visualization :: (SSDWidgetMonad t m) => m (Event t (m ()))
visualization = do
  verticalLayout def $ do
    results <- sequence
      [ menuButton "Icons" iconGui
      , menuButton "Tooltip" tooltipGui
      ]
    return $ leftmost results

dataInput :: (SSDWidgetMonad t m) => m (Event t (m ()))
dataInput = do
    accordion $ do
      accordionPanel def (span "Textual") $ do 
        verticalLayout def $ do
          textButton <- button def{_buttonConfig_label = "Text field"}
          return $ leftmost
            [ fmap (const textGui) $ _button_click textButton
            ]

checkboxGui :: (SSDWidgetMonad t m) => m ()
checkboxGui = do
  constSimpleElement "h3" "Basic usage"
  verticalLayout def{_orderedLayoutConfig_padding = constDyn True, _orderedLayoutConfig_style = vstyle} $ do
    void $ checkbox def{_checkboxConfig_label = "Option"} True
  where
    vstyle = constDyn
      [ ("box-shadow", "0 0 0 1px rgba(0, 0, 0, 0.1)")
      , ("width", "100%")
      ]

textGui :: (SSDWidgetMonad t m) => m ()
textGui = span "Text Gui"

initialGui :: (SSDWidgetMonad t m) => m ()
initialGui = 
  demoLayout 
  $ verticalLayout def{_orderedLayoutConfig_margin = constDyn True} 
  $ do
    constSimpleElement "h2" "Choose an element in the left menu"

iconGui :: (SSDWidgetMonad t m) => m ()
iconGui = ironIcon "vaadin:vaadin-h"  

demoLayout :: SSDWidgetMonad t m => m a -> m a
demoLayout = horizontalLayout def{ _orderedLayoutConfig_style = constDyn growStyles }

growStyles :: [(Text, Text)]
growStyles = 
  [ ("flex-grow", "1")
  , ("height", "100%")
  ]

tooltipGui :: (SSDWidgetMonad t m) => m ()
tooltipGui = span "Tooltip GUI!"

buttonExample :: (SSDWidgetMonad t m) => m ()
buttonExample = 
  verticalLayout def $ do
    exampleBasic
    -- exampleDisabled
    -- exampleIcon
  where
    exampleBasic = exampleWith "Basic Button" $ do
      horizontalLayout def $ do 
        btn1 <- button def{_buttonConfig_label = "Button"}
        clickCount <- count $ _button_click btn1
        content $ fmap (\c -> [i|clicked #{c} times.|]) clickCount

    exampleWith header content = do
      verticalLayout def $ do
        constSimpleElement "h2" header
        div $ content   

exampleUi :: (SSDWidgetMonad t m) => m (Event t ())
exampleUi = do
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
