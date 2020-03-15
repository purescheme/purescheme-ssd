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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Simple
  ( (!)
  , booleanAttribute
  , attribute
  , ssDomEventsAttribute
  , simpleName
  , simpleNodeElement
  , leafNode
  , simpleElement
  , leafElement
  )
where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML (Node(..), Element(..), Name(..), elementAttributes)

(!) :: Element -> Maybe (Name, Text) -> Element
(!) el Nothing = el
(!) el@Element{elementAttributes} (Just (k, v)) = el{elementAttributes = Map.insert k v elementAttributes}

booleanAttribute :: Name -> Bool -> Maybe (Name, Text)
booleanAttribute _ False = Nothing
booleanAttribute k True = Just (k, T.empty)

attribute :: Name -> Text -> Maybe (Name, Text)
attribute n v = Just (n, v)

ssDomEventsAttribute :: [Text] -> Maybe (Name, Text)
ssDomEventsAttribute [] = Nothing
ssDomEventsAttribute evs = attribute "data-reflex-vaadin-events" $ T.intercalate " " evs

simpleName :: Text -> Name
simpleName t = Name t Nothing Nothing

simpleNodeElement :: Text -> Text -> Node
simpleNodeElement name content = NodeElement $ Element (simpleName name) Map.empty [NodeContent content]

leafNode :: Text -> Node
leafNode name = NodeElement $ leafElement name

simpleElement :: Text -> Text -> Element
simpleElement name content = Element (simpleName name) Map.empty [NodeContent content]

leafElement :: Text -> Element
leafElement name = Element (simpleName name) Map.empty []
