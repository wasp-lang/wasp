module Wasp.Cli.Command.Watch
  ( WatchCompileResult (..),
    WatchEvent (..),
    watch,
  )
where

import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (tryPutMVar, tryTakeMVar)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import StrongPath (Abs, Dir, File', Path', Rel, (</>))
import qualified StrongPath as SP
import System.Directory (canonicalizePath)
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (CompileResult (..), compileIO, printCompilationResult)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Generator.Common as Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir, compileResultWarningsAndErrors)
import Wasp.Project.Common (srcDirInWaspProjectDir)

data WatchEvent
  = -- | As soon as a watched file changes, with the time of the change.
    FileChanged UTCTime
  | -- | Once a compile has finished, whether it succeeded or not.
    Recompiled WatchCompileResult

data WatchCompileResult = WatchCompileResult
  { _watchChangedProjectFiles :: [Path' (Rel WaspProjectDir) File'],
    _watchCompileResult :: CompileResult,
    -- | Changes from this time on aren't part of the compile.
    _watchCompileStartTime :: UTCTime
  }

-- TODO: Idea: Read .gitignore file, and ignore everything from it. This will then also cover the
--   .wasp dir, and users can easily add any custom stuff they want ignored. But, we also have to
--   be ready for the case when there is no .gitignore, that could be possible.

-- | Forever listens for any file changes at the very top level of
-- @waspProjectDir@, and also for any changes at any depth in the
-- @waspProjectDir@/src/ dir. If there is a change,
-- compiles Wasp source files in @waspProjectDir@ and regenerates files in
-- @outDir@. It will defer recompilation until no new change was detected in the
-- last second. It also takes 'ongoingCompilationResultMVar' MVar, into which it
-- stores the result (warnings, errors) of the latest (re)compile whenever it
-- happens. If there is already something in the MVar, it will get overwritten.
-- It reports what happens to 'onWatchEvent'.
watch ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  MVar ([CompileWarning], [CompileError]) ->
  (WatchEvent -> IO ()) ->
  IO ()
watch waspProjectDir outDir ongoingCompilationResultMVar onWatchEvent = do
  -- fsnotify reports paths under the canonical path of the watched dirs, which
  -- differs from the project dir's path if that goes through a symlink.
  canonicalWaspProjectDir <- canonicalizePath $ SP.fromAbsDir waspProjectDir
  FSN.withManager $ \mgr -> do
    chan <- newChan
    _ <- watchFilesAtTopLevelOfWaspProjectDir mgr chan
    _ <- watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan srcDirInWaspProjectDir
    listenForEvents canonicalWaspProjectDir chan =<< getCurrentTime
  where
    watchFilesAtTopLevelOfWaspProjectDir mgr chan =
      FSN.watchDir mgr (SP.fromAbsDir waspProjectDir) eventFilter (onEvent chan)
      where
        eventFilter event =
          isWatchedFile filename && filename /= "package-lock.json"
          where
            filename = FP.takeFileName $ FSN.eventPath event

    watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan dirInWaspProjectDir =
      FSN.watchTree mgr (SP.fromAbsDir $ waspProjectDir </> dirInWaspProjectDir) eventFilter (onEvent chan)
      where
        eventFilter = isWatchedFile . FP.takeFileName . FSN.eventPath

    onEvent :: Chan FSN.Event -> FSN.Event -> IO ()
    onEvent chan event = do
      onWatchEvent $ FileChanged $ FSN.eventTime event
      writeChan chan event

    -- TODO: Might be valuable to also filter out files from .gitignore.
    isWatchedFile :: String -> Bool
    isWatchedFile filename =
      not (isEditorTmpFile filename)
        && filename /= ".DS_Store"

    listenForEvents :: FilePath -> Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents canonicalWaspProjectDir chan lastCompileTime = do
      event <- readChan chan
      if isStaleEvent event lastCompileTime
        then -- Ignore delayed/stale events older than our last compile time.
          listenForEvents canonicalWaspProjectDir chan lastCompileTime
        else do
          -- Recompile, but only after a 1s period of no new events.
          events <- collectEventsUntilQuiet chan lastCompileTime 1 event
          currentTime <- getCurrentTime
          recompile currentTime $ mapMaybe (eventToProjectFile canonicalWaspProjectDir) events
          listenForEvents canonicalWaspProjectDir chan currentTime

    updateOngoingCompilationResultMVar :: ([CompileWarning], [CompileError]) -> IO ()
    updateOngoingCompilationResultMVar (warnings, errors) =
      -- Here we first ensure that MVar is empty, by taking from it if there is anything in it,
      -- and then we put into it the new compilation result.
      -- This is not atomic so in theory somebody could interject and put their own value
      -- just before we put the new value, but there is nobody else putting stuff into it
      -- at this moment so it will always be ok. We still use `tryPut` to avoid blocking in such
      -- case, even if only theoretical.
      tryTakeMVar ongoingCompilationResultMVar
        >> tryPutMVar ongoingCompilationResultMVar (warnings, errors)
        >> return ()

    -- Collects events until no new events are received for a duration of `secondsToDelay`.
    -- If a stale event arrives during an active timer window, we immediately
    -- return control to the caller.
    collectEventsUntilQuiet :: Chan FSN.Event -> UTCTime -> Int -> FSN.Event -> IO [FSN.Event]
    collectEventsUntilQuiet chan lastCompileTime secondsToDelay firstEvent =
      collectEvents [firstEvent]
      where
        collectEvents events = do
          eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
          case eventOrDelay of
            Left event
              | isStaleEvent event lastCompileTime -> return events
              | otherwise ->
                  -- We have a new event, restart waiting process.
                  collectEvents $ event : events
            Right () -> return events

    eventToProjectFile :: FilePath -> FSN.Event -> Maybe (Path' (Rel WaspProjectDir) File')
    eventToProjectFile canonicalWaspProjectDir event =
      SP.parseRelFile $ FP.normalise $ FP.makeRelative canonicalWaspProjectDir (FSN.eventPath event)

    isStaleEvent :: FSN.Event -> UTCTime -> Bool
    isStaleEvent event lastCompileTime = FSN.eventTime event < lastCompileTime
    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    recompile :: UTCTime -> [Path' (Rel WaspProjectDir) File'] -> IO ()
    recompile compileStartTime changedProjectFiles = do
      cliSendMessage $ Msg.Start "Recompiling on file change..."
      compileResult <- compileIO waspProjectDir outDir
      let (warnings, errors) = compileResultWarningsAndErrors compileResult

      printCompilationResult (warnings, errors)
      let watchCompileResult =
            WatchCompileResult
              { _watchChangedProjectFiles = changedProjectFiles,
                _watchCompileResult = compileResult,
                _watchCompileStartTime = compileStartTime
              }
      case _compileOutcome compileResult of
        Right _ -> cliSendMessage $ Msg.Success "Recompilation on file change succeeded."
        Left _ -> cliSendMessage (Msg.Failure "Recompilation on file change failed." $ show (length errors) ++ " errors found")
      updateOngoingCompilationResultMVar (warnings, errors)
      onWatchEvent $ Recompiled watchCompileResult

    -- TODO: This is a hardcoded approach to ignoring most of the common tmp files that editors
    --   create next to the source code. Bad thing here is that users can't modify this,
    --   so better approach would be probably to use information from .gitignore instead, or
    --   maybe combining the two somehow.
    isEditorTmpFile :: String -> Bool
    isEditorTmpFile filename =
      or
        [ ".#" `isPrefixOf` filename, -- Emacs lock files.
          "#" `isPrefixOf` filename && "#" `isSuffixOf` filename, -- Emacs auto-save files.
          "~" `isSuffixOf` filename, -- Emacs and vim backup files.
          "." `isPrefixOf` filename && ".swp" `isSuffixOf` filename, -- Vim swp files.
          "." `isPrefixOf` filename && ".un~" `isSuffixOf` filename -- Vim undo files.
        ]
