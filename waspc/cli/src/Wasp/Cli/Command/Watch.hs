module Wasp.Cli.Command.Watch
  ( watch,
  )
where

import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (tryPutMVar, tryTakeMVar)
import Control.Monad (unless)
import Data.List (isSuffixOf)
import Data.Time.Clock (UTCTime, getCurrentTime)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (compileIO, printCompilationResult)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Generator.Common as Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import Wasp.Project.Common (extPublicDirInWaspProjectDir, srcDirInWaspProjectDir)

-- TODO: Idea: Read .gitignore file, and ignore everything from it. This will then also cover the
--   .wasp dir, and users can easily add any custom stuff they want ignored. But, we also have to
--   be ready for the case when there is no .gitignore, that could be possible.

-- | Forever listens for any file changes at the very top level of
-- @waspProjectDir@, and also for any changes at any depth in the
-- @waspProjectDir@/src/ and @WaspProjectDir@/public dirs. If there is a change,
-- compiles Wasp source files in @waspProjectDir@ and regenerates files in
-- @outDir@. It will defer recompilation until no new change was detected in the
-- last second. It also takes 'ongoingCompilationResultMVar' MVar, into which it
-- stores the result (warnings, errors) of the latest (re)compile whenever it
-- happens. If there is already something in the MVar, it will get overwritten.
watch ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.ProjectRootDir) ->
  MVar ([CompileWarning], [CompileError]) ->
  IO ()
watch waspProjectDir outDir ongoingCompilationResultMVar = FSN.withManager $ \mgr -> do
  chan <- newChan
  _ <- watchFilesAtTopLevelOfWaspProjectDir mgr chan
  _ <- watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan srcDirInWaspProjectDir
  _ <- watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan extPublicDirInWaspProjectDir
  listenForEvents chan =<< getCurrentTime
  where
    watchFilesAtTopLevelOfWaspProjectDir mgr chan =
      FSN.watchDirChan mgr (SP.fromAbsDir waspProjectDir) eventFilter chan
      where
        eventFilter :: FSN.Event -> Bool
        eventFilter event =
          -- TODO: Might be valuable to also filter out files from .gitignore.
          not (isEditorTmpFile filename)
            && filename /= "package-lock.json"
          where
            filename = FP.takeFileName $ FSN.eventPath event

    watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan dirInWaspProjectDir =
      FSN.watchTreeChan mgr (SP.fromAbsDir $ waspProjectDir </> dirInWaspProjectDir) eventFilter chan
      where
        eventFilter :: FSN.Event -> Bool
        eventFilter event =
          -- TODO: Might be valuable to also filter out files from .gitignore.
          not (isEditorTmpFile filename)
          where
            filename = FP.takeFileName $ FSN.eventPath event

    listenForEvents :: Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents chan lastCompileTime = do
      event <- readChan chan
      if isStaleEvent event lastCompileTime
        then -- Ignore delayed/stale events older than our last compile time.
          listenForEvents chan lastCompileTime
        else do
          -- Recompile, but only after a 1s period of no new events.
          waitUntilNoNewEvents chan lastCompileTime 1
          currentTime <- getCurrentTime
          (warnings, errors) <- recompile
          updateOngoingCompilationResultMVar (warnings, errors)
          listenForEvents chan currentTime

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

    -- Blocks until no new events are recieved for a duration of `secondsToDelay`.
    -- Consumes any new events during an active timer window and then restarts wait.
    -- If a stale event comes in during an active timer window, we immediately
    -- return control to the caller.
    waitUntilNoNewEvents :: Chan FSN.Event -> UTCTime -> Int -> IO ()
    waitUntilNoNewEvents chan lastCompileTime secondsToDelay = do
      eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
      case eventOrDelay of
        Left event ->
          unless (isStaleEvent event lastCompileTime) $
            -- We have a new event, restart waiting process.
            waitUntilNoNewEvents chan lastCompileTime secondsToDelay
        Right () -> return ()

    isStaleEvent :: FSN.Event -> UTCTime -> Bool
    isStaleEvent event lastCompileTime = FSN.eventTime event < lastCompileTime
    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    recompile :: IO ([CompileWarning], [CompileError])
    recompile = do
      cliSendMessage $ Msg.Start "Recompiling on file change..."
      (warnings, errors) <- compileIO waspProjectDir outDir

      printCompilationResult (warnings, errors)
      if null errors
        then
          cliSendMessage $
            Msg.Success "Recompilation on file change succeeded."
        else
          cliSendMessage $
            Msg.Failure "Recompilation on file change failed." $
              show (length errors) ++ " errors found"

      return (warnings, errors)

    -- TODO: This is a hardcoded approach to ignoring most of the common tmp files that editors
    --   create next to the source code. Bad thing here is that users can't modify this,
    --   so better approach would be probably to use information from .gitignore instead, or
    --   maybe combining the two somehow.
    isEditorTmpFile :: String -> Bool
    isEditorTmpFile "" = False
    isEditorTmpFile filename =
      or
        [ take 2 filename == ".#", -- Emacs lock files.
          head filename == '#' && last filename == '#', -- Emacs auto-save files.
          last filename == '~', -- Emacs and vim backup files.
          head filename == '.' && ".swp" `isSuffixOf` filename, -- Vim swp files.
          head filename == '.' && ".un~" `isSuffixOf` filename -- Vim undo files.
        ]
