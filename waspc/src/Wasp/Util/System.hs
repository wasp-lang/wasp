{-# LANGUAGE TupleSections #-}

module Wasp.Util.System
  ( resolveExecNameIO,
    isSystemWindows,
    isSystemMacOS,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Extra (firstJustM)
import StrongPath (Abs, File', Path', parseAbsFile)
import System.Directory (findExecutable)
import qualified System.Info

-- | Resolve given executable name (e.g. `node`) to the version of the name that resolves
-- successfully and the corresponding full path to which it resolves.
-- Version, because we might try a couple of different versions of the name.
-- Throws IOError if it failed to resolve the name.
-- Example: resolveExecNameIO "npm" -> ("npm.cmd", "C:\...\npm.cmd")
resolveExecNameIO :: String -> IO (String, Path' Abs File')
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
