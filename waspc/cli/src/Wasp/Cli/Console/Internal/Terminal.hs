-- | The only module that works with stdout, stdin and stderr. Everything Wasp
-- writes to the terminal or reads from it goes through here. Only
-- "Wasp.Cli.Console" and the modules next to this one use it, to build what
-- the console hands out to the rest of the code.
--
-- A lint rule in @.hlint.yaml@ keeps the other modules away from the three
-- handles and from the functions that use them without naming them (e.g.
-- 'putStrLn'). See https://github.com/wasp-lang/wasp/issues/5493 for details.
module Wasp.Cli.Console.Internal.Terminal
  ( Terminal,
    newTerminal,
    writeLine,
    writeToStdout,
    writeRawOutput,
    showLiveLine,
    hideLiveLine,
    suspendLiveLine,
    resumeLiveLine,
    isInputTerminal,
    isOutputTerminal,
    setLineBufferingForOutput,
    writePrompt,
    readLine,
    isInputEOF,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (finally)
import Control.Monad (forever, unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (BufferMode (LineBuffering), Handle, hFlush, hIsTerminalDevice, hSetBuffering, stderr, stdin, stdout)
import System.IO.Error (ioeGetHandle, isEOFError)
import Wasp.Util.IO.Process (OutputStream (..))
import qualified Wasp.Util.TextStyle as TextStyle

-- | Everything that is written goes through its lock, so that two writers
-- never mix their output, and nothing is written over the live line.
newtype Terminal = Terminal (MVar TerminalState)

data TerminalState = TerminalState
  { -- | The texts of the live lines that were asked for and not hidden yet. Only
    -- the latest one is on the screen. The ones before it come back as the
    -- later ones get hidden.
    liveLineTexts :: [String],
    spinnerFrameIndex :: Int,
    -- | Redraws the live line to make its spinner turn. Runs for as long as
    -- there is a live line.
    spinner :: Maybe (Async ()),
    -- | See 'suspendLiveLine'.
    liveLineSuspensionsCount :: Int,
    isPromptOpen :: Bool,
    -- | While the live line is on the screen, a line is written out only
    -- once it's complete, because the live line needs a row of its own. These
    -- are the unfinished ends of what was written so far.
    unfinishedStdoutLine :: T.Text,
    unfinishedStderrLine :: T.Text
  }

newTerminal :: IO Terminal
newTerminal =
  Terminal
    <$> newMVar
      TerminalState
        { liveLineTexts = [],
          spinnerFrameIndex = 0,
          spinner = Nothing,
          liveLineSuspensionsCount = 0,
          isPromptOpen = False,
          unfinishedStdoutLine = "",
          unfinishedStderrLine = ""
        }

writeLine :: Terminal -> String -> IO ()
writeLine terminal = writeToStdout terminal . (++ "\n")

writeToStdout :: Terminal -> String -> IO ()
writeToStdout terminal = write terminal stdout . T.pack

writeRawOutput :: Terminal -> OutputStream -> T.Text -> IO ()
writeRawOutput terminal = write terminal . getOutputStreamHandle

getOutputStreamHandle :: OutputStream -> Handle
getOutputStreamHandle Stdout = stdout
getOutputStreamHandle Stderr = stderr

-- | While the live line is on the screen, the text goes above it.
write :: Terminal -> Handle -> T.Text -> IO ()
write terminal handle text = modifyState terminal $ \state ->
  if isLiveLineOnScreen state
    then do
      eraseLiveLine
      let (completeLines, unfinishedLine) = T.breakOnEnd "\n" $ getUnfinishedLine handle state <> text
      writeAndFlush handle completeLines
      drawLiveLine state
      return $ setUnfinishedLine handle unfinishedLine state
    else writeAndFlush handle text >> return state

-- | Shows the text on a line that stays the last one on the screen, with a
-- spinner in front of it, until 'hideLiveLine' is called. If a live line is
-- already there, the text takes its place until then.
showLiveLine :: Terminal -> String -> IO ()
showLiveLine terminal text = modifyState terminal $ \state -> do
  spinner <- maybe (Async.async $ turnSpinner terminal) return state.spinner
  changeLiveLine (return ()) state state {liveLineTexts = text : state.liveLineTexts, spinner = Just spinner}

-- | Takes the latest live line off the screen. Nothing of it stays there.
hideLiveLine :: Terminal -> IO ()
hideLiveLine terminal = modifyState terminal $ \state -> do
  let remainingLiveLineTexts = drop 1 state.liveLineTexts
  when (null remainingLiveLineTexts) $ mapM_ Async.cancel state.spinner
  changeLiveLine (return ()) state $
    state
      { liveLineTexts = remainingLiveLineTexts,
        spinner = if null remainingLiveLineTexts then Nothing else state.spinner
      }

-- | Takes the live line off the screen until 'resumeLiveLine' is called, for
-- the time in which something else than Wasp writes to the terminal or reads
-- from it. Whatever Wasp writes in that time is written out right away.
suspendLiveLine :: Terminal -> IO ()
suspendLiveLine terminal = modifyState terminal $ \state ->
  changeLiveLine (return ()) state state {liveLineSuspensionsCount = state.liveLineSuspensionsCount + 1}

resumeLiveLine :: Terminal -> IO ()
resumeLiveLine terminal = modifyState terminal $ \state ->
  changeLiveLine (return ()) state state {liveLineSuspensionsCount = state.liveLineSuspensionsCount - 1}

turnSpinner :: Terminal -> IO ()
turnSpinner terminal = forever $ do
  threadDelay spinnerFrameDurationInMicroseconds
  modifyState terminal $ \state -> do
    let stateWithNextFrame = state {spinnerFrameIndex = (state.spinnerFrameIndex + 1) `mod` length spinnerFrames}
    when (isLiveLineOnScreen stateWithNextFrame) $ eraseLiveLine >> drawLiveLine stateWithNextFrame
    return stateWithNextFrame
  where
    spinnerFrameDurationInMicroseconds = 80 * 1000

-- | Takes the live line of the current state off the screen, runs the action
-- and puts the live line of the new state on the screen. If the live line is
-- gone in the new state, a line that was waiting to be completed is written
-- out first.
changeLiveLine :: IO () -> TerminalState -> TerminalState -> IO TerminalState
changeLiveLine action currentState newState = do
  when (isLiveLineOnScreen currentState) eraseLiveLine
  if isLiveLineOnScreen newState
    then action >> drawLiveLine newState >> return newState
    else do
      writeAndFlush stdout newState.unfinishedStdoutLine
      writeAndFlush stderr newState.unfinishedStderrLine
      action
      return newState {unfinishedStdoutLine = "", unfinishedStderrLine = ""}

isLiveLineOnScreen :: TerminalState -> Bool
isLiveLineOnScreen state =
  not (null state.liveLineTexts) && state.liveLineSuspensionsCount == 0 && not state.isPromptOpen

drawLiveLine :: TerminalState -> IO ()
drawLiveLine state = case state.liveLineTexts of
  [] -> return ()
  latestText : _ -> writeAndFlush stdout $ T.pack $ TextStyle.styleAccent spinnerFrame ++ " " ++ latestText
  where
    spinnerFrame = spinnerFrames !! state.spinnerFrameIndex

-- | Moves the cursor to the start of the line it is on and erases that line.
eraseLiveLine :: IO ()
eraseLiveLine = writeAndFlush stdout $ T.pack $ "\r" ++ TextStyle.ansiEscapeCode ++ TextStyle.ansiEraseLineCode

spinnerFrames :: [String]
spinnerFrames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

getUnfinishedLine :: Handle -> TerminalState -> T.Text
getUnfinishedLine handle state
  | handle == stderr = state.unfinishedStderrLine
  | otherwise = state.unfinishedStdoutLine

setUnfinishedLine :: Handle -> T.Text -> TerminalState -> TerminalState
setUnfinishedLine handle unfinishedLine state
  | handle == stderr = state {unfinishedStderrLine = unfinishedLine}
  | otherwise = state {unfinishedStdoutLine = unfinishedLine}

writeAndFlush :: Handle -> T.Text -> IO ()
writeAndFlush handle text = unless (T.null text) $ T.IO.hPutStr handle text >> hFlush handle

modifyState :: Terminal -> (TerminalState -> IO TerminalState) -> IO ()
modifyState (Terminal stateVar) = modifyMVar_ stateVar

isInputTerminal :: IO Bool
isInputTerminal = hIsTerminalDevice stdin

isOutputTerminal :: IO Bool
isOutputTerminal = hIsTerminalDevice stdout

-- If we don't explicitly set line buffering, the output gets block-buffered
-- when stdout is not a terminal (e.g. redirected to a file or another program),
-- so messages from long-running commands don't show up until the command exits.
setLineBufferingForOutput :: IO ()
setLineBufferingForOutput = hSetBuffering stdout LineBuffering

-- | Writes a part of a prompt, which stays on the same line as the user's
-- answer, so it's written out right away. The live line is off the screen
-- from then on until the answer is read.
writePrompt :: Terminal -> String -> IO ()
writePrompt terminal text = modifyState terminal $ \state ->
  changeLiveLine (writeAndFlush stdout $ T.pack text) state state {isPromptOpen = True}

-- | Reads the user's answer. Reaching the end of the input while waiting for
-- it (Ctrl+D) throws an error that 'isInputEOF' recognizes.
readLine :: Terminal -> IO String
readLine terminal = getLine `finally` closePrompt
  where
    closePrompt = modifyState terminal $ \state -> changeLiveLine (return ()) state state {isPromptOpen = False}

isInputEOF :: IOError -> Bool
isInputEOF e = isEOFError e && ioeGetHandle e == Just stdin
