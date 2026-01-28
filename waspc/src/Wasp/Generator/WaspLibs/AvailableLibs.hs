module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

waspLibs :: [WaspLib.WaspLib]
waspLibs =
  WaspLib.makeWaspLib
    <$> [ -- NOTE: The package names of the libs should match the names in the
          -- `package.json` files of the libs in the ./libs directory.
          "@wasp.sh/lib-auth"
        ]
