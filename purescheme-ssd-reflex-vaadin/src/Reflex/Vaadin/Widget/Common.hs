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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Vaadin.Widget.Common where

import Text.XML.Simple

import Data.Text (Text)
import Reflex
import Text.XML

label :: (Reflex t, DynamicWriter t [Node] m) => Dynamic t Text -> m ()
label = tellDyn . fmap (pure . simpleNodeElement "span") 

jsScript :: (Reflex t, DynamicWriter t [Node] m) => Text -> m ()
jsScript scriptName = 
  tellDyn 
  $ constDyn 
  [ NodeElement 
    $ leafElement "script" 
    ! attribute "src" scriptName 
    ! attribute "type" "text/javascript"
  ]
