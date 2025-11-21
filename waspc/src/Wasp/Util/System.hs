module Wasp.Util.System
  ( isSystemWindows,
    isSystemMacOS,
    isEnvVarValueTruthy,
  )
where

import qualified System.Info

isSystemWindows :: Bool
isSystemWindows = System.Info.os == "mingw32"

isSystemMacOS :: Bool
isSystemMacOS = System.Info.os == "darwin"

isEnvVarValueTruthy :: String -> Bool
isEnvVarValueTruthy envVarValue = envVarValue `notElem` ["0", "false", "no", "off"]
