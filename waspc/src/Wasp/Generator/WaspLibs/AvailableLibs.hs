module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
    sharedWaspLibs,
    serverWaspLibs,
    webAppWaspLibs,
  )
where

import StrongPath (reldir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the <repo>/packages/libs directory.

-- | All the libs. The SDK depends on all of them.
waspLibs :: [WaspLib.WaspLib]
waspLibs = sharedWaspLibs ++ serverWaspLibs ++ webAppWaspLibs

-- | The libs that both the server and the web app use.
sharedWaspLibs :: [WaspLib.WaspLib]
sharedWaspLibs =
  [ WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|],
    WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|]
  ]

-- | The libs that only the server uses.
serverWaspLibs :: [WaspLib.WaspLib]
serverWaspLibs = []

-- | The libs that only the web app uses. The DevTools lib only runs in the
-- Vite dev server.
webAppWaspLibs :: [WaspLib.WaspLib]
webAppWaspLibs =
  [ WaspLib.makeWaspLib "@wasp/lib-devtools" [reldir|devtools|]
  ]
