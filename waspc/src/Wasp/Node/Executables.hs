module Wasp.Node.Executables
  ( nodeExec,
    npmExec,
    npxExec,
  )
where

import GHC.IO (unsafePerformIO)
import Wasp.Util.System (resolveExecNameIO)

{-# NOINLINE nodeExec #-}
nodeExec :: String
nodeExec = fst $ unsafePerformIO $ resolveExecNameIO "node"

{-# NOINLINE npmExec #-}
npmExec :: String
npmExec = fst $ unsafePerformIO $ resolveExecNameIO "npm"

{-# NOINLINE npxExec #-}
npxExec :: String
npxExec = fst $ unsafePerformIO $ resolveExecNameIO "npx"
