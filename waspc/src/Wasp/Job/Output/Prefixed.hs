{-# LANGUAGE TupleSections #-}

module Wasp.Job.Output.Prefixed
  ( JobOutput,
    printPrefixed,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit (ConduitT, await)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (hFlush)
import qualified Wasp.Job as Job
import Wasp.Job.Kind (JobKind)
import qualified Wasp.Job.Kind as Kind
import Wasp.Process (OutputStream (..))
import qualified Wasp.Util.Terminal as Term

-- | Output of a job, labeled with the kind of job that produced it.
type JobOutput = (JobKind, Job.Output)

-- | Prints the output of one or more jobs, prefixing each line with the job
-- it came from. See 'printOutputPrefixed' for how it is printed.
printPrefixed :: (MonadIO m) => ConduitT JobOutput o m ()
printPrefixed = printAll initialState
  where
    printAll state =
      await >>= \case
        Nothing -> return ()
        Just jobOutput -> printOutputPrefixed state jobOutput >>= printAll

    initialState =
      PrefixedWriterState
        { _outputsWithPendingNewline = S.empty,
          _lastOutput = Nothing
        }

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
printOutputPrefixed :: (MonadIO m) => PrefixedWriterState -> JobOutput -> m PrefixedWriterState
printOutputPrefixed (PrefixedWriterState outputsWithPendingNewline lastOutput) jobOutput = do
  let (outputsWithPendingNewline', messageContent) =
        applyPendingNewline outputsWithPendingNewline jobOutput
  let prefixedMessageContent = addPrefixWhereNeeded messageContent

  liftIO $ printPrefixedMessageContent prefixedMessageContent

  return $ PrefixedWriterState outputsWithPendingNewline' (Just jobOutput)
  where
    printPrefixedMessageContent :: T.Text -> IO ()
    printPrefixedMessageContent content = T.IO.hPutStr outHandle content >> hFlush outHandle
      where
        Job.Output stream _ = snd jobOutput
        outHandle = Job.getOutputHandle stream

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

        ensurePrefixAtStartIfNotContinuingOnSameOutput :: T.Text -> T.Text
        ensurePrefixAtStartIfNotContinuingOnSameOutput text =
          let continuingOnSameOutput =
                (getOutputChannel <$> lastOutput) == Just (getOutputChannel jobOutput)
              prefixAtStart =
                or [(delimiter <> prefix) `T.isPrefixOf` text | delimiter <- ["\r", "\n", ""]]
           in if not continuingOnSameOutput && not prefixAtStart then prefix <> text else text

        ensureNewlineAtStartIfInterruptingAnotherOutput :: T.Text -> T.Text
        ensureNewlineAtStartIfInterruptingAnotherOutput text =
          let interruptingAnotherOutput =
                (getOutputChannel <$> lastOutput) /= Just (getOutputChannel jobOutput)
              newlineAtStart = "\n" `T.isPrefixOf` text
           in if interruptingAnotherOutput && not newlineAtStart then "\n" <> text else text

        prefix :: T.Text
        prefix = makeOutputPrefix jobOutput

data PrefixedWriterState = PrefixedWriterState
  { _outputsWithPendingNewline :: !OutputsWithPendingNewline,
    _lastOutput :: !(Maybe JobOutput)
  }

-- Where a job message is printed to: the job and its output stream.
data OutputChannel = OutputChannel
  { _outputJobKind :: !Kind.JobKind,
    _outputIsStderr :: !Bool
  }
  deriving (Eq, Ord)

type OutputsWithPendingNewline = S.Set OutputChannel

-- | Given a set of job message outputs with pending newline and a job message,
-- it applies any pending newline (newline from the previous messages from the same output)
-- to the job message content while also detecting if content ends with a newline
-- and in that case adds it to the set of pending newlines (while removing used pending newline).
-- It returns this updated content and updated set of pending newlines.
applyPendingNewline ::
  OutputsWithPendingNewline -> JobOutput -> (OutputsWithPendingNewline, T.Text)
applyPendingNewline outputsWithPendingNewline jobOutput = (outputsWithPendingNewline', content')
  where
    content' = addPendingNewlineToStartIfAny $ removeTrailingNewlineIfAny content
      where
        removeTrailingNewlineIfAny = if contentEndsWithNewline then T.init else id
        addPendingNewlineToStartIfAny =
          if getOutputChannel jobOutput `S.member` outputsWithPendingNewline then ("\n" <>) else id

    outputsWithPendingNewline' = updateOp output outputsWithPendingNewline
      where
        updateOp = if contentEndsWithNewline then S.insert else S.delete

    contentEndsWithNewline = "\n" `T.isSuffixOf` content

    output = getOutputChannel jobOutput
    Job.Output _ content = snd jobOutput

getOutputChannel :: JobOutput -> OutputChannel
getOutputChannel jobOutput@(jobKind, _) =
  OutputChannel
    { _outputJobKind = jobKind,
      _outputIsStderr = isStderrOutput jobOutput
    }

makeOutputPrefix :: JobOutput -> T.Text
makeOutputPrefix jobOutput =
  T.pack . concatMap (\(text, styles) -> Term.applyStyles styles text) . concat $
    [ [(startDelimiter, jobStyles)],
      [unstyled namePaddingFront],
      [(jobName, jobStyles)],
      [unstyled namePaddingBack],
      styledFlags,
      [(endDelimiter, jobStyles)],
      [unstyled " "]
    ]
  where
    (namePaddingFront, namePaddingBack) =
      ( replicate namePaddingLengthFront ' ',
        replicate namePaddingLengthBack ' '
      )
      where
        namePaddingLengthFront = paddingLength `div` 2
        namePaddingLengthBack = paddingLength `div` 2 + paddingLength `mod` 2 - length (concatMap fst styledFlags)
        paddingLength = max 0 (minPrefixLength - numVisibleChars)
        numVisibleChars = length . concat $ [startDelimiter, jobName, endDelimiter]
        minPrefixLength = length $ startDelimiter <> " " <> longestJobName <> " " <> endDelimiter
        longestJobName =
          maximumBy (comparing length) $
            fst . getJobNameAndStyles <$> [(minBound :: JobKind) .. maxBound]

    (startDelimiter, endDelimiter) = ("[", "]")

    styledFlags :: [StyledText]
    styledFlags =
      [("!", [Term.Red, Term.Bold]) | isStderrOutput jobOutput]

    (jobName, jobStyles) = getJobNameAndStyles $ fst jobOutput

    getJobNameAndStyles = \case
      Kind.Wasp -> ("Wasp", [Term.Yellow])
      Kind.Server -> ("Server", [Term.Magenta])
      Kind.WebApp -> ("Client", [Term.Cyan])
      Kind.Db -> ("Db", [Term.Blue])

    unstyled = (,[])

type StyledText = (String, [Term.Style])

isStderrOutput :: JobOutput -> Bool
isStderrOutput (_, Job.Output stream _) = stream == Stderr
