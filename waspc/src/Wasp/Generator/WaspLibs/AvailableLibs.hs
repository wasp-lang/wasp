module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
    getWaspLibsForApp,
  )
where

import StrongPath (reldir)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- | Every lib Wasp ships, whether or not an app uses it.
waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ -- NOTE: The package names of the libs should match the names in the
    -- `package.json` files of the libs in the <repo>/packages/libs directory.
    WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|],
    WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|]
  ]

-- | The libs an app gets. Every app gets every lib for now; a lib only some
-- apps use can be left out here, so other apps don't install it.
getWaspLibsForApp :: AppSpec -> [WaspLib.WaspLib]
getWaspLibsForApp _spec = waspLibs
