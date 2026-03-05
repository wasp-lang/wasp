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

-- | This is a stub identical to the @setupLogger@ function from lsp 1.4.0.0
--
-- Updating GHC to 9.6.7 forced us to update lsp to a version newer than
-- 1.5.0.0.
--
-- Lsp 1.5.0.0 drops support for @hslogger@ and no longer exposes the
-- @setupLogger@ function. To properly accomodate the change, we would have to
-- update our entire logging architecture in the LSP.
--
-- We soon plan to move Wasp to the TS Spec. Spending time on the LSP therefore
-- makes little sense, so we've decided to stick with the "old approach" by
-- copying over the @setupLogger@ function verbatim and putting it here.
-- Reference:
-- https://hackage.haskell.org/package/lsp-1.4.0.0/docs/src/Language.LSP.Server.Core.html#setupLogger
--
-- To avoid our logs clashing with LSP's new default logs, we've also had to
-- disable the new logging mechanism in @Wasp.LSP.Server@.
--
-- These two small changes (creating a stub + disabling default logging) allow
-- us to keep the rest of the code completely intact without compromising any
-- functionality.
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
