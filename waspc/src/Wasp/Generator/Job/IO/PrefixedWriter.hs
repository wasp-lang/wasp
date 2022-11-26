{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Generator.Job.IO.PrefixedWriter
  ( printJobMessagePrefixed,
    runPrefixedWriter,
    PrefixedWriter,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, put)
import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (hFlush, stderr)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Common (getJobMessageContent, getJobMessageOutHandle)
import qualified Wasp.Util.Terminal as Term

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
-- Prefixes include job type name, and optional indication that output is stderr, e.g.:
-- "Server:", "Web app:", "Db(stderr):"
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
-- But, what we can at least do is avoid having this situation described above, where newline is
-- "kidnapped", and we do that by postponing the newline (make it pending) for later,
-- until the next message from the same output (job + output stream) arrives.
--
-- Specifically, what we do is postpone printing of a newline if it is the last character in a message.
-- We make it pending instead, and once the new message comes from the same output, we apply it at the
-- start of that message.
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
printJobMessagePrefixed :: J.JobMessage -> PrefixedWriter ()
printJobMessagePrefixed jobMessage = do
  (PrefixedWriterState outputsWithPendingNewline lastJobMessage) <- get

  let (outputsWithPendingNewline', messageContent) =
        applyPendingNewline outputsWithPendingNewline jobMessage
  let prefixedMessageContent = addPrefixWhereNeeded lastJobMessage messageContent

  put $ PrefixedWriterState outputsWithPendingNewline' (Just jobMessage)

  liftIO $ printPrefixedMessageContent prefixedMessageContent
  where
    printPrefixedMessageContent :: T.Text -> IO ()
    printPrefixedMessageContent content = T.IO.hPutStr outHandle content >> hFlush outHandle
      where
        outHandle = getJobMessageOutHandle jobMessage

    -- TODO: We haven't considered Windows much here, so in the future we might
    --   want to check that this works ok on Windows and tweak it a bit if not.
    addPrefixWhereNeeded :: Maybe J.JobMessage -> T.Text -> T.Text
    addPrefixWhereNeeded lastJobMessage =
      ensureNewlineAtStartIfInterruptingAnotherOutput
        . ensurePrefixAtStartIfNotContinuingOnSameOutput
        . addPrefixAfterSubstr "\r"
        . addPrefixAfterSubstr "\n"
      where
        addPrefixAfterSubstr :: T.Text -> T.Text -> T.Text
        addPrefixAfterSubstr substr = T.intercalate (substr <> prefix) . T.splitOn substr

        ensurePrefixAtStartIfNotContinuingOnSameOutput :: T.Text -> T.Text
        ensurePrefixAtStartIfNotContinuingOnSameOutput text =
          let continuingOnSameOutput =
                (getJobMessageOutput <$> lastJobMessage) == Just (getJobMessageOutput jobMessage)
              prefixAtStart =
                or [(delimiter <> prefix) `T.isPrefixOf` text | delimiter <- ["\r", "\n", ""]]
           in if not continuingOnSameOutput && not prefixAtStart then prefix <> text else text

        ensureNewlineAtStartIfInterruptingAnotherOutput :: T.Text -> T.Text
        ensureNewlineAtStartIfInterruptingAnotherOutput text =
          let interruptingAnotherOutput =
                (getJobMessageOutput <$> lastJobMessage) /= Just (getJobMessageOutput jobMessage)
              newlineAtStart = "\n" `T.isPrefixOf` text
           in if interruptingAnotherOutput && not newlineAtStart then "\n" <> text else text

        prefix :: T.Text
        prefix = makeJobMessagePrefix jobMessage

newtype PrefixedWriter a = PrefixedWriter {_runPrefixedWriter :: StateT PrefixedWriterState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState PrefixedWriterState)

data PrefixedWriterState = PrefixedWriterState
  { _outputsWithPendingNewline :: !OutputsWithPendingNewline,
    _lastJobMessage :: !(Maybe J.JobMessage)
  }

runPrefixedWriter :: PrefixedWriter a -> IO a
runPrefixedWriter pw = fst <$> runStateT (_runPrefixedWriter pw) initState
  where
    initState =
      PrefixedWriterState
        { _outputsWithPendingNewline = S.empty,
          _lastJobMessage = Nothing
        }

-- Job message output type.
data Output = Output
  { _outputJobType :: !J.JobType,
    _outputIsStderr :: !Bool
  }
  deriving (Eq, Ord)

type OutputsWithPendingNewline = S.Set Output

-- | Given a set of job message outputs with pending newline and a job message,
-- it applies any pending newline (newline from the previous messages from the same output)
-- to the job message content while also detecting if content ends with a newline
-- and in that case adds it to the set of pending newlines (while removing used pending newline).
-- It returns this updated content and updated set of pending newlines.
applyPendingNewline ::
  OutputsWithPendingNewline -> J.JobMessage -> (OutputsWithPendingNewline, T.Text)
applyPendingNewline outputsWithPendingNewline jobMessage = (outputsWithPendingNewline', content')
  where
    content' = addPendingNewlineToStartIfAny $ removeTrailingNewlineIfAny content
      where
        removeTrailingNewlineIfAny = if contentEndsWithNewline then T.init else id
        addPendingNewlineToStartIfAny =
          if getJobMessageOutput jobMessage `S.member` outputsWithPendingNewline then ("\n" <>) else id

    outputsWithPendingNewline' = updateOp output outputsWithPendingNewline
      where
        updateOp = if contentEndsWithNewline then S.insert else S.delete

    contentEndsWithNewline = "\n" `T.isSuffixOf` content

    output = getJobMessageOutput jobMessage
    content = getJobMessageContent jobMessage

getJobMessageOutput :: J.JobMessage -> Output
getJobMessageOutput jm =
  Output
    { _outputJobType = J._jobType jm,
      _outputIsStderr = getJobMessageOutHandle jm == stderr
    }

makeJobMessagePrefix :: J.JobMessage -> T.Text
makeJobMessagePrefix jobMsg =
  case J._jobType jobMsg of
    J.Server -> T.pack $ Term.applyStyles [Term.Magenta] "Server"
    J.WebApp -> T.pack $ Term.applyStyles [Term.Cyan] "Web app"
    J.Db -> T.pack $ Term.applyStyles [Term.Blue] "Db"
    <> ( if getJobMessageOutHandle jobMsg == stderr
           then T.pack $ Term.applyStyles [Term.Yellow] "(stderr)"
           else ""
       )
    <> ": "
