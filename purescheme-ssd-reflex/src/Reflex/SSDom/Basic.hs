{-# LANGUAGE OverloadedStrings #-}
module Reflex.SSDom.Basic
  ( span
  , small
  , div
  , constSimpleElement
  , jsScript
  , content
  )
where
  
import Reflex.SSDom

import Prelude hiding (span, div)

import qualified Data.Map as Map
import Data.Text (Text)
import Reflex
import Text.XML
import Text.XML.Simple

span :: SSDWidgetMonad t m => Dynamic t Text -> m ()
span = tellNodes . fmap (pure . simpleNodeElement "span") 

small :: SSDWidgetMonad t m => Dynamic t Text -> m ()
small = tellNodes . fmap (pure . simpleNodeElement "small") 

div :: SSDWidgetMonad t m => m a -> m a
div inner = do
  (result, innerHtml) <- runInner inner
  let 
    htmlDyn = do
      nInner <- innerHtml
      return [ NodeElement $ Element (simpleName "div") Map.empty nInner ]
  tellNodes htmlDyn
  return result

constSimpleElement :: SSDWidgetMonad t m => Text -> Text -> m ()
constSimpleElement name content = 
  tellNodes $ constDyn [ NodeElement $ simpleElement name content ]

jsScript :: SSDWidgetMonad t m => Text -> m ()
jsScript scriptName = 
  tellNodes 
  $ constDyn 
  [ NodeElement 
    $ leafElement "script" 
    ! attribute "src" scriptName 
    ! attribute "type" "module"
  ]

content :: SSDWidgetMonad t m => Dynamic t Text -> m ()
content inner = 
  let
    htmlDyn = do
      nInner <- inner
      return [ NodeContent nInner]
  in
    tellNodes htmlDyn
