{-# OPTIONS_GHC -Wno-deprecations #-}

module Wasp.Util.Debug
  ( inDebugMode,
    debugTrace,
  )
where

import Debug.Pretty.Simple (pTrace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

inDebugMode :: Bool
inDebugMode =
  case unsafePerformIO $ lookupEnv "DEBUG" of
    Nothing -> False
    _ -> True

debugTrace :: String -> a -> a
debugTrace output a =
  if inDebugMode
    then pTrace output a
    else a
