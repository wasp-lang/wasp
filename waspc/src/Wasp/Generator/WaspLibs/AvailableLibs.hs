module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
    serverWaspLibs,
  )
where

import StrongPath (reldir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the <repo>/packages/libs directory.

-- | All the libs. The SDK depends on all of them.
waspLibs :: [WaspLib.WaspLib]
waspLibs =
  serverWaspLibs
    ++ [WaspLib.makeWaspLib "@wasp/lib-devtools" [reldir|devtools|]]

-- | The libs the server depends on. That's all but the DevTools lib, which
-- only the Vite dev server uses.
serverWaspLibs :: [WaspLib.WaspLib]
serverWaspLibs =
  [ WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|],
    WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|]
  ]
