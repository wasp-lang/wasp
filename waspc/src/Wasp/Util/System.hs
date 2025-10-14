{-# LANGUAGE TupleSections #-}

module Wasp.Util.System
  ( resolveExecNameIO,
    isSystemWindows,
    isSystemMacOS,
    ExecName,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Extra (firstJustM)
import StrongPath (Abs, File', Path', parseAbsFile)
import System.Directory (findExecutable)
import qualified System.Info

-- | Executable name as expected by Haskell's "System.Process" and its 'System.Process.RawCommand',
-- therefore suited for passing to their functions for creating/executing processes.
-- It can be just "node", or "node.exe", or relative or full path, ... .
type ExecName = FilePath

-- | Resolve given executable name (e.g. "node") to the version of the name that resolves
-- successfully and the corresponding full path to which it resolves.
--
-- "Version of the name" because we might try a couple of different versions of the name till we
-- find one that resolves (e.g. for "node" we might also try "node.cmd" and "node.exe" on Windows).
--
-- The resolved path corresponds to the program that would be executed by
-- 'System.Process.createProcess' if exec name was provided as 'System.Process.RawCommand'. Check
-- 'System.Process.findExecutable' for more details since that is what we use internally.
--
-- Motivation for this function was mainly driven by how exec names are resolved when executing a
-- process on Windows.
-- On Linux/MacOS situation is simple, the system will normally do the name resolution for us, so
-- e.g. if we pass "npm" to 'System.Process.proc', that will work out of the box.
-- But on Windows, that will normally fail, since there is no "npm" really but instead "npm.cmd" or
-- "npm.exe". In that case, we want to figure out what exactly is the right exec name to use.
-- Note that we don't have to bother with this when using 'System.Process.shell' instead of
-- 'System.Process.proc', but at the price of abandoning any argument escaping.
--
-- Throws IOError if it failed to resolve the name.
--
-- Example: resolveExecNameIO "npm" -> ("npm.cmd", "C:\...\npm.cmd")
resolveExecNameIO :: ExecName -> IO (ExecName, Path' Abs File')
resolveExecNameIO execName = do
  firstJustM (\name -> ((name,) <$>) <$> findExecutable name) execNamesToLookForByPriorityDesc >>= \case
    Just (execName', execPath) -> (execName',) <$> parseAbsFile execPath
    Nothing ->
      (throwIO . userError . unlines)
        [ "Could not find '" <> execName <> "' executable on your system.",
          "Please ensure " <> execName <> " is installed and available in your PATH."
        ]
  where
    execNamesToLookForByPriorityDesc
      | isSystemWindows = (execName <>) <$> ["", ".cmd", ".exe", ".ps1"]
      | otherwise = [execName]

isSystemWindows :: Bool
isSystemWindows = System.Info.os == "mingw32"

isSystemMacOS :: Bool
isSystemMacOS = System.Info.os == "darwin"
