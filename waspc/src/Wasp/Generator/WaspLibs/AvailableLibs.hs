module Wasp.Generator.WaspLibs.AvailableLibs
  ( allWaspLibs,
    sharedWaspLibs,
    serverWaspLibs,
    webAppWaspLibs,
  )
where

import Data.List (nub)
import StrongPath (reldir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the <repo>/packages/libs directory.

-- | All the libs. The SDK depends on all of them.
allWaspLibs :: [WaspLib.WaspLib]
allWaspLibs = nub $ sharedWaspLibs ++ serverWaspLibs ++ webAppWaspLibs

serverWaspLibs :: [WaspLib.WaspLib]
serverWaspLibs = sharedWaspLibs

webAppWaspLibs :: [WaspLib.WaspLib]
webAppWaspLibs =
  sharedWaspLibs
    ++ [ WaspLib.makeWaspLib "@wasp/lib-devtools" [reldir|devtools|],
         WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|]
       ]

-- | The libs that both the server and the web app use.
sharedWaspLibs :: [WaspLib.WaspLib]
sharedWaspLibs =
  [ WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|]
  ]
