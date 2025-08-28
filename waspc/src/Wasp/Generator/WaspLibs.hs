module Wasp.Generator.WaspLibs
  ( getWaspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the ./libs directory.
getWaspLibs :: IO [WaspLib.WaspLib]
getWaspLibs = sequence [WaspLib.makeWaspLib "@wasp.sh/libs-auth"]
