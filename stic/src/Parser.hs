module Parser (
    parseWasp
) where

import Wasp (Wasp(..), WaspElement(..))

parseWasp :: String -> Wasp
parseWasp fileContent = Wasp [WaspElementPage]
