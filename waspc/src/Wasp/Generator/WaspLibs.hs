module Wasp.Generator.WaspLibs
  ( waspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ WaspLib.makeWaspLib "@wasp.sh/libs-auth"
  ]
