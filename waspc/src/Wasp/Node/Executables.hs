module Wasp.Node.Executables
  ( nodeExec,
    npmExec,
    npxExec,
  )
where

import GHC.IO (unsafePerformIO)
import StrongPath (fromAbsFile)
import Wasp.Util.System (ExecName, resolveExecNameIO)

{-# NOINLINE nodeExec #-} -- To ensure single execution, for unsafePerformIO.
nodeExec :: ExecName
nodeExec = fromAbsFile $ unsafePerformIO $ resolveExecNameIO "node"

{-# NOINLINE npmExec #-} -- To ensure single execution, for unsafePerformIO.
npmExec :: ExecName
npmExec = fromAbsFile $ unsafePerformIO $ resolveExecNameIO "npm"

{-# NOINLINE npxExec #-} -- To ensure single execution, for unsafePerformIO.
npxExec :: ExecName
npxExec = fromAbsFile $ unsafePerformIO $ resolveExecNameIO "npx"
