{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Wasp.Job.IO.PrefixedWriter
  ( printJobMessagePrefixed,
    formatJobMessage,
    runPrefixedWriter,
    PrefixedWriter,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (get, put)
import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (hFlush, stderr)
import Wasp.Job (JobType)
import qualified Wasp.Job as J
import Wasp.Job.Common (getJobMessageContent, getJobMessageOutHandle)
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
-- Print newlines as they arrive, but wait for text before printing the next prefix.
-- Otherwise, ordinary CLI output can appear after an unused prefix:
--   Server: First line
--   Server: Some other output!
--
-- Delaying the newline as well would join that output onto the previous line.
-- Keeping the newline and delaying only the prefix gives:
--   Server: First line
--   Some other output!
--   Server: Second line
--
-- We also track the previous job and output stream. When another output interrupts
-- an unfinished line, we start a new line and print the new output's prefix.
printJobMessagePrefixed :: J.JobMessage -> PrefixedWriter ()
printJobMessagePrefixed jobMessage =
  unless (T.null $ getJobMessageContent jobMessage) $ do
    lastJobMessage <- get
    let prefixedMessageContent = formatJobMessage lastJobMessage jobMessage
    put $ Just jobMessage
    liftIO $ printPrefixedMessageContent prefixedMessageContent
  where
    printPrefixedMessageContent :: T.Text -> IO ()
    printPrefixedMessageContent content = T.IO.hPutStr outHandle content >> hFlush outHandle
      where
        outHandle = getJobMessageOutHandle jobMessage

-- TODO: We haven't considered Windows much here, so in the future we might
--   want to check that this works ok on Windows and tweak it a bit if not.
formatJobMessage :: Maybe J.JobMessage -> J.JobMessage -> T.Text
formatJobMessage previous jobMessage
  | T.null content = ""
  | otherwise = separator <> prefixLines needsPrefix content
  where
    content = getJobMessageContent jobMessage
    sameOutput = (getJobMessageOutput <$> previous) == Just (getJobMessageOutput jobMessage)
    atLineStart = maybe True (endsWithLineBreak . getJobMessageContent) previous
    previousLineComplete = maybe True (T.isSuffixOf "\n" . getJobMessageContent) previous
    needsPrefix = atLineStart || not sameOutput
    separator = if not sameOutput && not previousLineComplete && not (T.isPrefixOf "\n" content) then "\n" else ""
    prefix = makeJobMessagePrefix jobMessage

    prefixLines addPrefix text =
      let (line, rest) = T.break isLineBreak text
          prefixedLine = if addPrefix && not (T.null line) then prefix <> line else line
       in prefixedLine <> case T.uncons rest of
            Nothing -> ""
            Just (delimiter, remaining) -> T.singleton delimiter <> prefixLines True remaining

    endsWithLineBreak text = maybe True (isLineBreak . snd) $ T.unsnoc text
    isLineBreak char = char == '\n' || char == '\r'

newtype PrefixedWriter a = PrefixedWriter {_runPrefixedWriter :: StateT (Maybe J.JobMessage) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Maybe J.JobMessage))

runPrefixedWriter :: PrefixedWriter a -> IO a
runPrefixedWriter pw = fst <$> runStateT (_runPrefixedWriter pw) Nothing

-- Job message output type.
data Output = Output
  { _outputJobType :: !J.JobType,
    _outputIsStderr :: !Bool
  }
  deriving (Eq)

getJobMessageOutput :: J.JobMessage -> Output
getJobMessageOutput jm =
  Output
    { _outputJobType = J._jobType jm,
      _outputIsStderr = getJobMessageOutHandle jm == stderr
    }

makeJobMessagePrefix :: J.JobMessage -> T.Text
makeJobMessagePrefix jobMsg =
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
            fst . getJobNameAndStyles <$> [(minBound :: JobType) .. maxBound]

    (startDelimiter, endDelimiter) = ("[", "]")

    styledFlags :: [StyledText]
    styledFlags =
      [("!", [Term.Red, Term.Bold]) | getJobMessageOutHandle jobMsg == stderr]

    (jobName, jobStyles) = getJobNameAndStyles $ J._jobType jobMsg

    getJobNameAndStyles = \case
      J.Wasp -> ("Wasp", [Term.Yellow])
      J.Server -> ("Server", [Term.Magenta])
      J.WebApp -> ("Client", [Term.Cyan])
      J.Db -> ("Db", [Term.Blue])

    unstyled = (,[])

type StyledText = (String, [Term.Style])
