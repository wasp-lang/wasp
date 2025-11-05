-- | Extra utilities for the LSP library.
--
-- This module contains functions that were removed from later versions of the
-- lsp package but are still useful for compatibility.
module Language.LSP.Extra
  ( setupLogger,
  )
where

import qualified Control.Exception as E
import Control.Monad (forM_)
import System.IO (Handle, IOMode (AppendMode), hClose, hPutStr, hSetEncoding, openFile, stderr, utf8)
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import System.Log.Logger (Priority)
import qualified System.Log.Logger as L

-- | Setup logger. Adapted from lsp-1.4.0.0.
setupLogger :: Maybe FilePath -> [String] -> Priority -> IO ()
setupLogger mLogFile extraLogNames level = do
  logStream <- case mLogFile of
    Just logFile -> openFile logFile AppendMode `E.catch` handleIOException logFile
    Nothing -> return stderr
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle = logH {LHS.closeFunc = hClose}
      logFormatter = L.tfLogFormatter logDateFormat logFormat
      logHandler = LH.setFormatter logHandle logFormatter

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger "haskell-lsp" $ L.setHandlers [logHandler]
  L.updateGlobalLogger "haskell-lsp" $ L.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    L.updateGlobalLogger logName $ L.setHandlers [logHandler]
    L.updateGlobalLogger logName $ L.setLevel level
  where
    logFormat = "$time [$tid] $prio $loggername:\t$msg"
    logDateFormat = "%Y-%m-%d %H:%M:%S%Q"

handleIOException :: FilePath -> E.IOException -> IO Handle
handleIOException logFile _ = do
  hPutStr stderr $ "Couldn't open log file " ++ logFile ++ "; falling back to stderr logging"
  return stderr
