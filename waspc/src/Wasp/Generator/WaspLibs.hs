module Wasp.Generator.WaspLibs
  ( getWaspLibs,
  )
where

import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

getWaspLibs :: IO [WaspLib.WaspLib]
getWaspLibs = sequence [WaspLib.makeWaspLib "@wasp.sh/libs-auth"]
