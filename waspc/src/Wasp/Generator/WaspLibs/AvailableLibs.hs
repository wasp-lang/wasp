module Wasp.Generator.WaspLibs.AvailableLibs
  ( makeWaspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

makeWaspLibs :: IO [WaspLib.WaspLib]
makeWaspLibs =
  mapM
    WaspLib.makeWaspLib
    [ -- NOTE: The package names of the libs should match the names in the
      -- `package.json` files of the libs in the ./libs directory.
      "@wasp.sh/lib-auth"
    ]
