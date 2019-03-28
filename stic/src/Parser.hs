module Parser
    ( parseWasp
    ) where

import qualified Wasp

parseWasp :: String -> Wasp.Wasp
parseWasp fileContent = Wasp.fromApp (Wasp.App "TestApp" "App title")
