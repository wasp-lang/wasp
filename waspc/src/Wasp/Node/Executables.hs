module Wasp.Node.Executables
  ( nodeExec,
    npmExec,
    npxExec,
  )
where

import GHC.IO (unsafePerformIO)
import StrongPath (fromAbsFile)
import Wasp.Util.System (resolveExecNameIO)

{-# NOINLINE nodeExec #-}
nodeExec :: String
nodeExec = fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "node"

{-# NOINLINE npmExec #-}
npmExec :: String
npmExec = fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "npm"

{-# NOINLINE npxExec #-}
npxExec :: String
npxExec = fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "npx"
