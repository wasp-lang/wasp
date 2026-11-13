{-# LANGUAGE FlexibleInstances #-}

module Wasp.Util.Interactive
  ( InteractiveTerminal,
    getInteractiveTerminal,
    isPromptInputEOF,
    runTaskWithSpinner,
    StatusLine,
    withStatusLine,
    OutputSource (..),
    writeOutput,
    OutputTrail (..),
    initialOutputTrail,
    separate,
    endOutputBlock,
    askForInput,
    askToChoose,
    askForRequiredInput,
    tryGettingConfirmation,
    askForConfirmation,
    tryGettingConfirmationWithTimeout,
    IsOption (..),
    ConfirmationError (..),
    Option (..),
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever, unless)
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (Handle, hFlush, hGetLine, hIsTerminalDevice, hPutStr, hPutStrLn, stdin, stdout)
import System.IO.Error (ioeGetHandle, isEOFError)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Wasp.Util (millisecondsToMicroSeconds)
import qualified Wasp.Util.Terminal as Term

-- This is the only way to get an interactive terminal.
-- The caller can decide what to do if they can't get one: fail with a way to
-- pass the answers as arguments, skip the question, etc.
getInteractiveTerminal :: IO (Maybe InteractiveTerminal)
getInteractiveTerminal = do
  isTerminal <- hIsTerminalDevice stdioTerminal.inputHandle
  return $
    if isTerminal
      -- If you change the terminal here, change it in `isPromptInputEOF` too.
      then Just stdioTerminal
      else Nothing

-- | True for an error caused by the user ending an input (Ctrl+D) while a
-- prompt is waiting for an answer.
isPromptInputEOF :: IOError -> Bool
-- If you change the terminal here, change it in `getInteractiveTerminal` too.
isPromptInputEOF e = isEOFError e && ioeGetHandle e == Just stdioTerminal.inputHandle

{-
  Why are we doing this?
  Using a list of Strings for options results in the Strings being wrapped in quotes
  when printed.

  What we want to avoid:
    Choose an option:
    - "one"
    - "two"

  What we want:
    Choose an option:
    - one
    - two

  We want to avoid this so users can type the name of the option when answering
  without having to type the quotes as well.

  We introduced the IsOption class to get different "show" behavior for Strings and other
  types. If we are using something other then String, an instance of IsOption needs to be defined,
  but for Strings it just returns the String itself.
-}
class IsOption o where
  showOption :: o -> String
  showOptionDescription :: o -> Maybe String

instance IsOption [Char] where
  showOption = id
  showOptionDescription = const Nothing

data Option o = Option
  { oDisplayName :: !String,
    oDescription :: !(Maybe String),
    oValue :: !o
  }

instance IsOption (Option o) where
  showOption = oDisplayName
  showOptionDescription = oDescription

data InteractiveTerminal = InteractiveTerminal
  { inputHandle :: Handle,
    outputHandle :: Handle,
    -- | The status line currently shown, if any. A terminal shows at most one.
    statusLineRef :: IORef (Maybe StatusLine)
  }

askForRequiredInput :: InteractiveTerminal -> String -> IO String
askForRequiredInput terminal question = repeatIfNull terminal $ askForInput terminal question

-- | Asks a yes/no question. Only "y" or "yes" (case-insensitive) count as yes.
askForConfirmation :: InteractiveTerminal -> String -> IO Bool
askForConfirmation terminal question = do
  answer <- askForInput terminal (question <> " [y/N]")
  return $ map toLower answer `elem` ["y", "yes"]

askToChoose :: forall o. (IsOption o) => InteractiveTerminal -> String -> NonEmpty o -> IO o
askToChoose _ _ (singleOption :| []) = return singleOption
askToChoose terminal question options = do
  hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Bold] question
  hPutStrLn terminal.outputHandle showIndexedOptions
  answer <- prompt terminal
  getOptionMatchingAnswer answer & maybe printErrorAndAskAgain return
  where
    getOptionMatchingAnswer :: String -> Maybe o
    getOptionMatchingAnswer "" = Just defaultOption
    getOptionMatchingAnswer answer =
      getOptionByIndex answer <|> getOptionByName answer

    getOptionByIndex :: String -> Maybe o
    getOptionByIndex idxStr =
      case readMaybe idxStr of
        Just idx | idx >= 1 && idx <= length options -> Just $ options NE.!! (idx - 1)
        _invalidIndex -> Nothing

    getOptionByName :: String -> Maybe o
    getOptionByName name = find ((== name) . showOption) options

    printErrorAndAskAgain :: IO o
    printErrorAndAskAgain = do
      hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Red] "Invalid selection. Type the name or the index of the option."
      askToChoose terminal question options

    showIndexedOptions :: String
    showIndexedOptions = intercalate "\n" $ showIndexedOption <$> zip [1 ..] (NE.toList options)
      where
        showIndexedOption (idx, option) =
          concat
            [ indexPrefix,
              optionName,
              tags,
              optionDescription
            ]
          where
            indexPrefix = Term.applyStyles [Term.Yellow] (showIndex idx) <> " "
            optionName = Term.applyStyles [Term.Bold] (showOption option)
            tags = whenDefault (Term.applyStyles [Term.Yellow] " (default)")
            optionDescription = showDescription (idx, option)
            whenDefault xs = if isDefaultOption option then xs else mempty

        showIndex idx = "[" ++ show (idx :: Int) ++ "]"

        showDescription (idx, option) = case showOptionDescription option of
          Just description -> "\n" <> replicate indentLength ' ' <> description
          Nothing -> ""
          where
            indentLength = length (showIndex idx) + 1

    defaultOption :: o
    defaultOption = NE.head options

    isDefaultOption :: o -> Bool
    isDefaultOption option = showOption option == showOption defaultOption

askForInput :: InteractiveTerminal -> String -> IO String
askForInput terminal question = do
  hPutStr terminal.outputHandle $ Term.applyStyles [Term.Bold] question
  prompt terminal

tryGettingConfirmation :: InteractiveTerminal -> String -> String -> IO (Either ConfirmationError ())
tryGettingConfirmation terminal message requiredAnswer =
  askForInput terminal message <&> \actualAnswer ->
    if actualAnswer == requiredAnswer
      then Right ()
      else Left $ WrongAnswer actualAnswer

tryGettingConfirmationWithTimeout :: InteractiveTerminal -> String -> String -> Int -> IO (Either ConfirmationError ())
tryGettingConfirmationWithTimeout terminal message requiredAnswer timeoutSeconds =
  timeout timeoutMicroseconds (tryGettingConfirmation terminal message requiredAnswer)
    <&> fromMaybe (Left Timeout)
  where
    timeoutMicroseconds = timeoutSeconds * 10 ^ (6 :: Int)

data ConfirmationError = Timeout | WrongAnswer String

repeatIfNull :: (Foldable t) => InteractiveTerminal -> IO (t a) -> IO (t a)
repeatIfNull terminal action = repeatUntil terminal null "This field cannot be empty." action

repeatUntil :: InteractiveTerminal -> (a -> Bool) -> String -> IO a -> IO a
repeatUntil terminal predicate errorMessage action = do
  result <- action
  if predicate result
    then do
      hPutStrLn terminal.outputHandle $ Term.applyStyles [Term.Red] errorMessage
      repeatUntil terminal predicate errorMessage action
    else return result

-- | Runs the action while showing the message as a status line with a
-- spinning glyph. The line is redrawn in place and erased once the action
-- finishes, so nothing of it stays in the terminal. While a status line is
-- already shown, the message takes its place for the action's duration
-- instead, so steps inside a bigger step share its line.
runTaskWithSpinner :: InteractiveTerminal -> String -> IO a -> IO a
runTaskWithSpinner terminal spinnerMessage action =
  readIORef terminal.statusLineRef >>= \case
    Just statusLine -> withStatusLineText statusLine (\groupText -> groupText ++ ": " ++ spinnerMessage) action
    Nothing -> withStatusLine terminal spinnerMessage (const action)

-- | Who is writing: Wasp itself (its messages, and the output of tools it
-- runs inline), or the running app's jobs with their prefixed logs.
data OutputSource = WaspMessage | JobLog
  deriving (Eq, Show)

-- | Writes output where everything else is written. While a status line is
-- shown, the output goes above it and the line is redrawn underneath, so
-- the two never share a row. Wasp's own output and the job logs are set
-- apart by an empty line, see 'separate'. Every writer of the process goes
-- through here.
writeOutput :: OutputSource -> Handle -> T.Text -> IO ()
writeOutput source handle output = do
  separated <- atomicModifyIORef' stdioOutputTrailRef $ \trail -> separate trail source output
  readIORef stdioStatusLineRef >>= \case
    Nothing -> T.IO.hPutStr handle separated >> hFlush handle
    Just statusLine -> modifyMVar_ statusLine.statusLineState $ \state -> do
      clearStatusLine statusLine state
      -- A line the writer has not finished yet stays just above the status
      -- line, and is redrawn with it, until the rest of it arrives.
      let (completed, unfinished) = T.breakOnEnd "\n" $ state.unfinishedLine <> separated
      T.IO.hPutStr handle completed >> hFlush handle
      let state' = state {unfinishedLine = unfinished}
      drawStatusLine statusLine state'
      return state'

-- | What the output written so far ends with: who wrote last, how many line
-- breaks are at the end, and whether Wasp's next output starts a new block.
data OutputTrail = OutputTrail
  { lastSource :: Maybe OutputSource,
    trailingLineBreaks :: Int,
    blockEnded :: Bool
  }
  deriving (Eq, Show)

initialOutputTrail :: OutputTrail
initialOutputTrail = OutputTrail Nothing 0 False

-- | Ends the current block of Wasp's output: whatever Wasp writes next starts
-- after an empty line, even if the job logs go on in between.
endOutputBlock :: IO ()
endOutputBlock = atomicModifyIORef' stdioOutputTrailRef $ \trail -> (trail {blockEnded = True}, ())

-- | Puts an empty line between Wasp's own output and the job logs, in either
-- direction, and before Wasp's output after 'endOutputBlock', unless one is
-- already there. Output that is nothing but line breaks is such an empty
-- line itself, so it never gets another one.
separate :: OutputTrail -> OutputSource -> T.Text -> (OutputTrail, T.Text)
separate trail source output = (trail', separator <> output)
  where
    separator
      | onlyLineBreaks || trail.trailingLineBreaks >= 2 = ""
      | isJust trail.lastSource && trail.lastSource /= Just source = "\n"
      | trail.blockEnded && source == WaspMessage = "\n"
      | otherwise = ""
    trail' =
      OutputTrail
        { lastSource = Just source,
          trailingLineBreaks =
            if onlyLineBreaks
              then trail.trailingLineBreaks + T.length output
              else T.length (T.takeWhileEnd (== '\n') output),
          blockEnded = trail.blockEnded && (source /= WaspMessage || onlyLineBreaks)
        }
    onlyLineBreaks = T.all (== '\n') output

-- | The line at the bottom of the terminal that is redrawn in place: a
-- spinning glyph and a text the running action can change. Anything written
-- while it is shown goes above it, through 'writeOutput'.
data StatusLine = StatusLine
  { statusLineTerminal :: InteractiveTerminal,
    -- | Holding the state also serializes drawing the line and writing above it.
    statusLineState :: MVar StatusLineState
  }

data StatusLineState = StatusLineState
  { text :: String,
    frame :: String,
    -- | Output written above the line that has no line break yet.
    unfinishedLine :: T.Text
  }

withStatusLine :: InteractiveTerminal -> String -> (StatusLine -> IO a) -> IO a
withStatusLine terminal initialText action = do
  statusLine <- StatusLine terminal <$> newMVar (StatusLineState initialText (head spinnerFrames) "")
  writeIORef terminal.statusLineRef (Just statusLine)
  Async.withAsync (spin statusLine) (const $ action statusLine)
    `finally` (writeIORef terminal.statusLineRef Nothing >> removeStatusLine statusLine)
  where
    spin statusLine = forever $
      forM_ spinnerFrames $ \nextFrame -> do
        modifyMVar_ statusLine.statusLineState $ \state -> do
          let state' = state {frame = nextFrame}
          drawSpinnerRow statusLine state'
          return state'
        threadDelay $ millisecondsToMicroSeconds 80

    spinnerFrames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

-- | Shows the text derived from the current one while the action runs, then
-- the current text again.
withStatusLineText :: StatusLine -> (String -> String) -> IO a -> IO a
withStatusLineText statusLine deriveText action = do
  previousText <- (.text) <$> readMVar statusLine.statusLineState
  setText $ deriveText previousText
  action `finally` setText previousText
  where
    setText t = modifyMVar_ statusLine.statusLineState $ \state -> return state {text = t}

-- | Draws the status line from the current row: the unfinished output line,
-- if any, then the spinner row below it.
drawStatusLine :: StatusLine -> StatusLineState -> IO ()
drawStatusLine statusLine state = do
  unless (T.null state.unfinishedLine)
    $ putStrFlush statusLine.statusLineTerminal
    $ T.unpack state.unfinishedLine ++ "\n"
  drawSpinnerRow statusLine state

-- | Redraws the spinner row in place.
drawSpinnerRow :: StatusLine -> StatusLineState -> IO ()
drawSpinnerRow statusLine state =
  putStrFlush statusLine.statusLineTerminal $
    clearLine ++ Term.applyStyles [Term.Yellow] state.frame ++ " " ++ state.text

-- | Erases what 'drawStatusLine' drew, leaving the cursor where it started.
clearStatusLine :: StatusLine -> StatusLineState -> IO ()
clearStatusLine statusLine state =
  putStrFlush statusLine.statusLineTerminal $
    clearLine ++ (if T.null state.unfinishedLine then "" else cursorUp ++ clearLine)

-- | Erases the status line for good, leaving any unfinished output line in
-- place for its writer to continue.
removeStatusLine :: StatusLine -> IO ()
removeStatusLine statusLine = withMVar statusLine.statusLineState $ \state -> do
  clearStatusLine statusLine state
  putStrFlush statusLine.statusLineTerminal $ T.unpack state.unfinishedLine

-- | Moves the cursor to the start of the line and erases the line.
clearLine :: String
clearLine = "\r" ++ Term.ansiEscapeCode ++ Term.ansiClearLineCode

cursorUp :: String
cursorUp = Term.ansiEscapeCode ++ "[1A"

-- | Prompts for the user's answer and reads it.. Reaching the end of input
-- while waiting for an answer (Ctrl+D) throws an error 'isPromptInputEOF'
-- recognizes.
prompt :: InteractiveTerminal -> IO String
prompt terminal = do
  putStrFlush terminal $ Term.applyStyles [Term.Yellow] " ▸ "
  T.unpack . T.strip . T.pack <$> hGetLine terminal.inputHandle

-- Explicit flush ensures prompt messages are printed immediately on all systems.
putStrFlush :: InteractiveTerminal -> String -> IO ()
putStrFlush terminal msg = do
  hPutStr terminal.outputHandle msg
  hFlush terminal.outputHandle

stdioTerminal :: InteractiveTerminal
stdioTerminal = InteractiveTerminal {inputHandle = stdin, outputHandle = stdout, statusLineRef = stdioStatusLineRef}

-- The one status line of the one terminal, so that steps started anywhere in
-- the process find the line a bigger step is showing.
{-# NOINLINE stdioStatusLineRef #-}
stdioStatusLineRef :: IORef (Maybe StatusLine)
stdioStatusLineRef = unsafePerformIO $ newIORef Nothing

{-# NOINLINE stdioOutputTrailRef #-}
stdioOutputTrailRef :: IORef OutputTrail
stdioOutputTrailRef = unsafePerformIO $ newIORef initialOutputTrail
