module Wasp.Util.Process
  ( -- * Executable Resolution
    -- These functions resolve executable names to their full paths on Windows,
    -- where executables often have extensions like .exe, .cmd, .bat.
    -- On other platforms, they return the original name unchanged.
    resolveExecutableIO,
    resolveExecutable,
  )
where

import Control.Exception (throwIO)
import Data.Function.Memoize (memoize)
import System.Directory (findExecutable)
import qualified System.Info
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)

-- | Resolve an executable name to its full path on Windows.
-- On non-Windows platforms, returns the original name unchanged.
--
-- This is the IO version that does the actual work. Use 'resolveExecutable'
-- for the memoized pure version.
--
-- If the executable cannot be found on Windows, throws an informative error.
resolveExecutableIO :: String -> IO String
resolveExecutableIO cmd
  | System.Info.os == "mingw32" = findWindowsExecutable cmd
  | otherwise = return cmd

-- | Find a Windows executable by searching PATH and trying common extensions.
-- Throws an informative error if the executable cannot be found.
findWindowsExecutable :: String -> IO String
findWindowsExecutable cmd = do
  findExecutable cmd >>= \case
    Just path -> return path
    Nothing ->
      throwIO . userError $
        unlines
          [ "Could not find '" ++ cmd ++ "' executable on your system.",
            "Please ensure " ++ cmd ++ " is installed and available in your PATH.",
            "On Windows, you may need to restart your terminal after installation."
          ]

-- | Memoized version of 'resolveExecutableIO'.
-- This is safe to use as a pure function because:
-- 1. Executable locations don't change during program execution
-- 2. The result is deterministic for a given executable name
-- 3. Any exceptions thrown are also memoized, so behavior is consistent
--
-- Use this version at call sites for convenience:
--
-- > P.proc (resolveExecutable "npm") ["install"]
{-# NOINLINE resolveExecutable #-}
resolveExecutable :: String -> String
resolveExecutable = memoize $ \cmd -> unsafePerformIO (resolveExecutableIO cmd)
