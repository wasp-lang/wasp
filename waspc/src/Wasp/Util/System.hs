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

-- | Resolve given executable name (e.g. "node") to the full executable path.
--
-- Note that filename of the resolved path might not be exactly equal to the provided executable
-- name, but may have additional extension (e.g. "node" might resolve to "/some/path/node.cmd").
--
-- The reason why we return resolved absolute executable path and not just the resolved filename
-- (e.g. "node.cmd" for "node") is that, as per Haskell docs, when passing just the filename to
-- 'System.Process.createProcess', we can get unexpected resolution if 'cwd' option is set.
-- For example it can resolve to "npm.cmd" in the local ".node_modules" instead of a global one.
-- So it is better to stick with absolute paths.
--
-- Motivation for this function was mainly driven by how exec names are resolved when executing a
-- process on Windows.
-- On Linux/MacOS situation is simple, the system will normally do the name resolution for us, so
-- e.g. if we pass "npm" to 'System.Process.proc', that will work out of the box.
-- But on Windows, that will normally fail, since there is no "npm" really but instead "npm.cmd" or
-- "npm.exe". In that case, we want to figure out what exactly is the right exec name to use.
-- Note that we don't have to bother with this when using 'System.Process.shell' instead of
-- 'System.Process.proc', becuase then the shell will do system resolution for us,
-- but at the price of abandoning any argument escaping.
--
-- Throws IOError if it failed to resolve the name.
--
-- Example: resolveExecNameIO "npm" -> "C:\...\npm.cmd"
resolveExecNameIO :: ExecName -> IO (Path' Abs File')
resolveExecNameIO execName = do
  firstJustM findExecutable execNamesToLookForByPriorityDesc >>= \case
    Just execPath -> parseAbsFile execPath
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
