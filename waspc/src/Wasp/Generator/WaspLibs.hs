module Wasp.Generator.WaspLibs
  ( initWaspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the ./libs directory.
initWaspLibs :: IO [WaspLib.WaspLib]
initWaspLibs = sequence [WaspLib.makeWaspLib "@wasp.sh/lib-auth"]
