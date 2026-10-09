{-# LANGUAGE TupleSections #-}

module Wasp.Job.Printer
  ( Printer,
    newPrinter,
    printOutput,
  )
where

import Control.Concurrent (MVar, modifyMVar_, newMVar)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (Handle, hFlush, stderr, stdout)
import qualified Wasp.Job.Common as J
import qualified Wasp.Util.Terminal as Term

-- | Prints job output to Wasp's own stdout and stderr. Jobs can print from
-- several threads at once, so it prints one write at a time.
newtype Printer = Printer (MVar PrefixedState)

newPrinter :: IO Printer
newPrinter = Printer <$> newMVar initialPrefixedState

-- | Prints the output as is if it has no job kind, or with the job kind's
-- prefix otherwise.
printOutput :: Printer -> Maybe J.JobType -> J.OutputType -> T.Text -> IO ()
printOutput (Printer stateVar) maybeJobType outputType content =
  modifyMVar_ stateVar $ \state -> do
    let (state', text) = case maybeJobType of
          Nothing -> (state, content)
          Just jobType -> prefixOutput (JobOutput jobType outputType) content state
    T.IO.hPutStr handle text >> hFlush handle
    return state'
  where
    handle = outputTypeHandle outputType

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
-- This is what this function does, it properly prefixes the given message.
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
prefixOutput :: JobOutput -> T.Text -> PrefixedState -> (PrefixedState, T.Text)
prefixOutput jobOutput content (PrefixedState outputsWithPendingNewline lastJobOutput) =
  ( PrefixedState outputsWithPendingNewline' (Just jobOutput),
    addPrefixWhereNeeded messageContent
  )
  where
    (outputsWithPendingNewline', messageContent) =
      applyPendingNewline outputsWithPendingNewline jobOutput content

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
          let prefixAtStart =
                or [(delimiter <> prefix) `T.isPrefixOf` text | delimiter <- ["\r", "\n", ""]]
           in if not continuingOnSameOutput && not prefixAtStart then prefix <> text else text

        ensureNewlineAtStartIfInterruptingAnotherOutput :: T.Text -> T.Text
        ensureNewlineAtStartIfInterruptingAnotherOutput text =
          let newlineAtStart = "\n" `T.isPrefixOf` text
           in if not continuingOnSameOutput && not newlineAtStart then "\n" <> text else text

        continuingOnSameOutput = lastJobOutput == Just jobOutput

        prefix :: T.Text
        prefix = makePrefix jobOutput

data PrefixedState = PrefixedState
  { _outputsWithPendingNewline :: !OutputsWithPendingNewline,
    _lastJobOutput :: !(Maybe JobOutput)
  }

initialPrefixedState :: PrefixedState
initialPrefixedState =
  PrefixedState
    { _outputsWithPendingNewline = S.empty,
      _lastJobOutput = Nothing
    }

-- | Where a message comes from: which job, and which of its streams.
data JobOutput = JobOutput
  { _jobOutputType :: !J.JobType,
    _jobOutputStream :: !J.OutputType
  }
  deriving (Eq, Ord)

type OutputsWithPendingNewline = S.Set JobOutput

-- | Given a set of job outputs with pending newline and a message,
-- it applies any pending newline (newline from the previous messages from the same output)
-- to the message content while also detecting if content ends with a newline
-- and in that case adds it to the set of pending newlines (while removing used pending newline).
-- It returns this updated content and updated set of pending newlines.
applyPendingNewline ::
  OutputsWithPendingNewline -> JobOutput -> T.Text -> (OutputsWithPendingNewline, T.Text)
applyPendingNewline outputsWithPendingNewline jobOutput content = (outputsWithPendingNewline', content')
  where
    content' = addPendingNewlineToStartIfAny $ removeTrailingNewlineIfAny content
      where
        removeTrailingNewlineIfAny = if contentEndsWithNewline then T.init else id
        addPendingNewlineToStartIfAny =
          if jobOutput `S.member` outputsWithPendingNewline then ("\n" <>) else id

    outputsWithPendingNewline' = updateOp jobOutput outputsWithPendingNewline
      where
        updateOp = if contentEndsWithNewline then S.insert else S.delete

    contentEndsWithNewline = "\n" `T.isSuffixOf` content

makePrefix :: JobOutput -> T.Text
makePrefix jobOutput =
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
            fst . getJobNameAndStyles <$> [(minBound :: J.JobType) .. maxBound]

    (startDelimiter, endDelimiter) = ("[", "]")

    styledFlags :: [StyledText]
    styledFlags =
      [("!", [Term.Red, Term.Bold]) | _jobOutputStream jobOutput == J.Stderr]

    (jobName, jobStyles) = getJobNameAndStyles $ _jobOutputType jobOutput

    getJobNameAndStyles = \case
      J.Wasp -> ("Wasp", [Term.Yellow])
      J.Server -> ("Server", [Term.Magenta])
      J.WebApp -> ("Client", [Term.Cyan])
      J.Db -> ("Db", [Term.Blue])

    unstyled = (,[])

type StyledText = (String, [Term.Style])

outputTypeHandle :: J.OutputType -> Handle
outputTypeHandle J.Stdout = stdout
outputTypeHandle J.Stderr = stderr
