module Wasp.Node.Executables
  ( nodeExec,
    npmExec,
    npxExec,
  )
where

import GHC.IO (unsafePerformIO)
import StrongPath (fromAbsFile)
import Wasp.Util.System (ExecName, resolveExecNameIO)

-- | Node executable name to be passed to Haskell's "System.Process" functions.
--
-- This function being top level form in combo with NOINLINE guarantees that IO action will get
-- executed only once per lifetime of the Haskell program.
{-# NOINLINE nodeExec #-}
nodeExec :: ExecName
nodeExec =
  -- NOTE: We are taking whole resolved absolute path here because just using the resolved exec name
  -- was still flaky on Windows in some situations.
  fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "node"

-- | Npm executable name to be passed to Haskell's "System.Process" functions.
--
-- This function being top level form in combo with NOINLINE guarantees that IO action will get
-- executed only once per lifetime of the Haskell program.
{-# NOINLINE npmExec #-}
npmExec :: ExecName
npmExec =
  -- NOTE: We are taking whole resolved absolute path here because just using the resolved exec name
  -- was still flaky on Windows in some situations.
  fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "npm"

-- | Node executable name to be passed to Haskell's "System.Process" functions.
--
-- This function being top level form in combo with NOINLINE guarantees that IO action will get
-- executed only once per lifetime of the Haskell program.
{-# NOINLINE npxExec #-}
npxExec :: ExecName
npxExec =
  -- NOTE: We are taking whole resolved absolute path here because just using the resolved exec name
  -- was still flaky on Windows in some situations.
  fromAbsFile $ snd $ unsafePerformIO $ resolveExecNameIO "npx"
