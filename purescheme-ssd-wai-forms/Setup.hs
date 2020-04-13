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
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup

import System.Process (callCommand)

main = defaultMainWithHooks $ simpleUserHooks { preConf = runNpm }

runNpm :: Args -> ConfigFlags -> IO HookedBuildInfo
runNpm _ _ = do
  callCommand "npm install --prefix js"
  callCommand "npm run prod --prefix js"
  return emptyHookedBuildInfo
