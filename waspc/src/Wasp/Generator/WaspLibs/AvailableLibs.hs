module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- | List of all available Wasp libraries.
-- Pure list - no IO needed since we're no longer computing checksums.
waspLibs :: [WaspLib.WaspLib]
waspLibs =
  map
    WaspLib.makeWaspLib
    [ -- NOTE: The package names of the libs should match the names in the
      -- `package.json` files of the libs in the ./libs directory.
      "@wasp.sh/lib-auth"
    ]
