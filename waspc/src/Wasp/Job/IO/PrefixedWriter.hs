{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Wasp.Job.IO.PrefixedWriter
  ( PrefixedWriter,
    newPrefixedWriter,
    printOutput,
    Output (..),
    formatOutput,
    PrefixedWriterState,
    initialPrefixedWriterState,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, put)
import Control.Monad.State.Strict (MonadState, StateT, execStateT)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.IO (Handle, stderr, stdout)
import qualified Wasp.Job.Common as J
import qualified Wasp.Util.Interactive as Interactive
import qualified Wasp.Util.Terminal as Term

-- | Prints job output to Wasp's own stdout and stderr. Jobs can print from
-- several threads at once, so it prints one message at a time.
newtype PrefixedWriter = PrefixedWriter (MVar PrefixedWriterState)

newPrefixedWriter :: IO PrefixedWriter
newPrefixedWriter = PrefixedWriter <$> newMVar initialPrefixedWriterState

-- | Prints the output as is if it has no job type, or prefixed with the job
-- type otherwise.
printOutput :: PrefixedWriter -> Maybe J.JobType -> J.OutputType -> T.Text -> IO ()
printOutput (PrefixedWriter stateVar) maybeJobType outputType content =
  modifyMVar_ stateVar $ execStateT $ _runPrefixedWriterM $ case maybeJobType of
    Nothing -> liftIO $ printMessageContent outputType content
    Just jobType -> do
      time <- liftIO $ formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
      printJobMessagePrefixed time (Output jobType outputType) content

-- |
-- Imagine you have a job sending following two messages:
--  1. "First"
--  2. " line\n"
--  3. "Second line"
--
-- What we want is to prefix new lines with a corresponding job prefix before we print them,
-- e.g. "Server: ". So we want to output this as:
--   Server: First line
--   Server: Second line
--
-- This is what this function does, it properly prefixes the given message and then prints it.
-- Prefixes include the time, the job type name, and optional indication that output is stderr, e.g.:
-- "15:18:42 [server]", "15:18:42 [client]"; stderr output has the time in bold red.
--
-- * Implementation details:
--
-- Simplest (and naive) way to go about this is to add prefix after any newline.
-- However, what can happen is that third message from above comes quite later than the second
-- message, and in the meantime some other output (e.g. from another job) is printed.
-- In such case, we get following:
--   Server: First line
--   Server:
--   Some other output!
--   Second line
--
-- This is not an easy problem to solve, as we can't know what kind of message is coming next,
-- and there are always situations where some other output might interrupt us.
-- We avoid this by printing the newline immediately, but postponing the next prefix
-- until another message from the same output arrives. Holding back the newline as
-- well would make ordinary CLI messages join the previous line.
--
-- This way we get proper output in the situation as described above:
--   Server: First line
--   Some other output!
--   Server: Second line
--
-- We additionaly check if the last message (before the current message) was from the same output.
-- If not, or there was no previous message, then we ensure there is prefix at the start of
-- the message. This helps with situations where output from one job was interrupted by the
-- output from another job, or when message is the very first message.
printJobMessagePrefixed :: String -> Output -> T.Text -> PrefixedWriterM ()
printJobMessagePrefixed time output content = do
  writerState <- get
  let (writerState', prefixedContent) = formatOutput time writerState output content
  put writerState'
  liftIO $ printMessageContent (_outputType output) prefixedContent

formatOutput :: String -> PrefixedWriterState -> Output -> T.Text -> (PrefixedWriterState, T.Text)
formatOutput time (PrefixedWriterState outputsWithPendingPrefix lastOutput) output content =
  (PrefixedWriterState outputsWithPendingPrefix' (Just output), prefixedMessageContent)
  where
    prefix = makeJobMessagePrefix time output
    (outputsWithPendingPrefix', messageContent) =
      applyPendingPrefix prefix outputsWithPendingPrefix output content
    trailingNewline = if "\n" `T.isSuffixOf` content then "\n" else ""
    prefixedMessageContent = addPrefixWhereNeeded messageContent <> trailingNewline

    -- TODO: We haven't considered Windows much here, so in the future we might
    --   want to check that this works ok on Windows and tweak it a bit if not.
    addPrefixWhereNeeded :: T.Text -> T.Text
    addPrefixWhereNeeded =
      ensureNewlineAtStartIfInterruptingAnotherOutput
        . ensurePrefixAtStartIfNotContinuingOnSameOutput
        . addPrefixAfterSubstr "\r"
        . addPrefixAfterSubstr "\n"
      where
        addPrefixAfterSubstr :: T.Text -> T.Text -> T.Text
        addPrefixAfterSubstr substr = T.intercalate (substr <> prefix) . T.splitOn substr

        -- A message that starts with a line break and then the prefix counts as
        -- prefixed only while that line break is needed to finish another
        -- output's line; after a complete line it is an empty line of this
        -- output and gets its own prefix.
        ensurePrefixAtStartIfNotContinuingOnSameOutput :: T.Text -> T.Text
        ensurePrefixAtStartIfNotContinuingOnSameOutput text =
          let continuingOnSameOutput = lastOutput == Just output
              prefixAtStart =
                prefix `T.isPrefixOf` text
                  || (not previousLineComplete && or [(delimiter <> prefix) `T.isPrefixOf` text | delimiter <- ["\r", "\n"]])
           in if not continuingOnSameOutput && not prefixAtStart then prefix <> text else text

        ensureNewlineAtStartIfInterruptingAnotherOutput :: T.Text -> T.Text
        ensureNewlineAtStartIfInterruptingAnotherOutput text =
          let interruptingAnotherOutput = lastOutput /= Just output
              newlineAtStart = "\n" `T.isPrefixOf` text
           in if interruptingAnotherOutput && not newlineAtStart && not previousLineComplete then "\n" <> text else text

        previousLineComplete = maybe False (`S.member` outputsWithPendingPrefix) lastOutput

newtype PrefixedWriterM a = PrefixedWriterM {_runPrefixedWriterM :: StateT PrefixedWriterState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState PrefixedWriterState)

data PrefixedWriterState = PrefixedWriterState
  { _outputsWithPendingPrefix :: !OutputsWithPendingPrefix,
    _lastOutput :: !(Maybe Output)
  }

initialPrefixedWriterState :: PrefixedWriterState
initialPrefixedWriterState =
  PrefixedWriterState
    { _outputsWithPendingPrefix = S.empty,
      _lastOutput = Nothing
    }

-- Job message output type.
data Output = Output
  { _outputJobType :: !J.JobType,
    _outputType :: !J.OutputType
  }
  deriving (Eq, Ord)

type OutputsWithPendingPrefix = S.Set Output

-- | Removes the final newline while adding prefixes, and adds the prefix owed
-- from the previous message. The caller prints the final newline immediately
-- after formatting, without leaving an unused prefix on the next line.
applyPendingPrefix ::
  T.Text -> OutputsWithPendingPrefix -> Output -> T.Text -> (OutputsWithPendingPrefix, T.Text)
applyPendingPrefix prefix outputsWithPendingPrefix output content = (outputsWithPendingPrefix', content')
  where
    content' = addPendingPrefixToStartIfAny $ removeTrailingNewlineIfAny content
      where
        removeTrailingNewlineIfAny = if contentEndsWithNewline then T.init else id
        addPendingPrefixToStartIfAny =
          if output `S.member` outputsWithPendingPrefix then (prefix <>) else id

    outputsWithPendingPrefix' = updateOp output outputsWithPendingPrefix
      where
        updateOp = if contentEndsWithNewline then S.insert else S.delete

    contentEndsWithNewline = "\n" `T.isSuffixOf` content

makeJobMessagePrefix :: String -> Output -> T.Text
makeJobMessagePrefix time output =
  T.pack $ Term.applyStyles timeStyles time ++ " " ++ Term.applyStyles jobStyles ("[" ++ jobName ++ "]") ++ " "
  where
    timeStyles = case _outputType output of
      J.Stdout -> [Term.Grey]
      J.Stderr -> [Term.Red, Term.Bold]
    (jobName, jobStyles) = case _outputJobType output of
      J.Server -> ("server", [Term.Magenta])
      J.WebApp -> ("client", [Term.Cyan])
      J.Db -> ("db", [Term.Blue])

printMessageContent :: J.OutputType -> T.Text -> IO ()
printMessageContent outputType = Interactive.writeOutput (getOutputTypeHandle outputType)

getOutputTypeHandle :: J.OutputType -> Handle
getOutputTypeHandle J.Stdout = stdout
getOutputTypeHandle J.Stderr = stderr
