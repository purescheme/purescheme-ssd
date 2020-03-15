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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.XML.Delta where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Algorithm.Diff
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import GHC.Generics (Generic)
import Text.Blaze
import qualified Text.XML as Xml

-- import Text.Blaze.Renderer.String (renderMarkup)
import Text.Blaze.Renderer.Text (renderMarkup)

data Delta
  = RemoveNode Int
  | AddNode Int Xml.Node
  | InnerHtml Int L.Text
  | PutAttribute Int Xml.Name Text
  | RemoveAttribute Int Xml.Name
  | ModifyChildren Int [Delta]
  deriving (Show, Generic)

instance ToJSON Delta

instance ToJSON Xml.Name where
  toJSON (Xml.Name name Nothing Nothing) = Aeson.String name
  toJSON _ = error "Names with namespace or prefix not supported"

instance ToJSONKey Xml.Name

instance ToJSON Xml.Node where
  toJSON (Xml.NodeElement (Xml.Element name attr inner)) =
    object 
      [ "type" .= Aeson.String "element"
      , "nodeName" .= toJSON name
      , "attributes" .= toJSON attr
      , "innerHtml" .= renderText inner
      ]
  toJSON (Xml.NodeContent content) =
    object
      [ "type" .= Aeson.String "text"
      , "content" .= Aeson.String content
      ]
  toJSON (Xml.NodeInstruction _) = error "NodeInstruction not supported"
  toJSON (Xml.NodeComment _) = error "NodeComment not supported"

renderText :: [Xml.Node] -> L.Text
renderText nodes = renderMarkup $ mconcat $ fmap toMarkup nodes

delta :: [Diff Xml.Node] -> [Delta]
delta di = reverse $ snd $ foldr go (0 :: Int, []) di
  where 
    go diffResult (ind, accum) =
      case diffResult of
        First _ -> (ind, RemoveNode ind:accum)
        Second b -> (ind + 1, AddNode ind b:accum)
        Both a b -> 
          if a == b
            then (ind + 1, accum)
            else 
              case (a, b) of
                (Xml.NodeElement (Xml.Element _ attr1 inner1), Xml.NodeElement (Xml.Element _ attr2 inner2)) -> 
                  if isCompleteReplacement inner1 inner2
                    then (ind + 1, InnerHtml ind (renderText inner2):( diffAttrs attr1 attr2 ind ++ accum))
                    else (ind + 1, modifyChildren ind (delta $ diff inner1 inner2) ++ diffAttrs attr1 attr2 ind ++ accum)
                _ -> error "Unexpected nodes to compare"

    modifyChildren ind delta' =
      if null delta'
        then mempty
        else pure $ ModifyChildren ind delta'

diffAttrs :: Map.Map Xml.Name Text -> Map.Map Xml.Name Text -> Int -> [Delta]
diffAttrs orig target ind = 
  removedAttributes ++ putAttributes
  where
    removedAttributes = fmap (\(k, _) -> RemoveAttribute ind k) $ Map.toList $ Map.difference orig target
    putAttributes = addedAttributes ++ changedAttributes
    addedAttributes = fmap (uncurry $ PutAttribute ind) $ Map.toList $ Map.difference target orig
    changedAttributes = mapMaybe snd $ Map.toList $ Map.intersectionWithKey toPutIfEquals orig target
    toPutIfEquals k o t = 
      if o == t
        then Nothing
        else Just (PutAttribute ind k t)

isCompleteReplacement :: [Xml.Node] -> [Xml.Node] -> Bool
isCompleteReplacement nodes1 nodes2 = all (not . isBoth) (diff nodes1 nodes2) 

isBoth :: Diff a -> Bool
isBoth (Both _ _) = True
isBoth _ = False

diff :: [Xml.Node] -> [Xml.Node] -> [Diff Xml.Node]
diff = getDiffBy compareNode
  where

    compareNode :: Xml.Node -> Xml.Node -> Bool
    compareNode el1@(Xml.NodeElement (Xml.Element name attr _)) el2@(Xml.NodeElement (Xml.Element name' attr' _)) =
      case (Map.lookup "id" attr, Map.lookup "id" attr') of
        (Just id1, Just id2) -> (name == name') && (id1 == id2)
        (Nothing, Nothing) -> name == name'
        _ -> el1 == el2
        
    compareNode node node' =
      node == node'

