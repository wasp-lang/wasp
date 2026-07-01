module Wasp.Cli.Command.Watch
  ( WatchCompileHooks (..),
    watch,
  )
where

import Control.Concurrent (MVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.MVar (tryPutMVar, tryTakeMVar)
import Data.List (isSuffixOf)
import Data.Time.Clock (UTCTime, getCurrentTime)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (compileIO, printCompilationResult)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Generator.Common as Wasp.Generator
import Wasp.Generator.ServerGenerator.Start
  ( ServerChangeImpact (..),
  )
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import Wasp.Project.Common (srcDirInWaspProjectDir)

newtype ChangeBatch = ChangeBatch [FSN.Event]

data WatchCompileHooks = WatchCompileHooks
  { _onSuccessfulCompile :: ServerChangeImpact -> IO (),
    _onFailedCompile :: IO ()
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
watch ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.GeneratedAppDir) ->
  MVar ([CompileWarning], [CompileError]) ->
  WatchCompileHooks ->
  IO ()
watch waspProjectDir outDir ongoingCompilationResultMVar watchCompileHooks = FSN.withManager $ \mgr -> do
  chan <- newChan
  _ <- watchFilesAtTopLevelOfWaspProjectDir mgr chan
  _ <- watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan srcDirInWaspProjectDir
  listenForEvents chan =<< getCurrentTime
  where
    watchFilesAtTopLevelOfWaspProjectDir mgr chan =
      FSN.watchDirChan mgr (SP.fromAbsDir waspProjectDir) eventFilter chan
      where
        eventFilter event =
          isWatchedFile filename && filename /= "package-lock.json"
          where
            filename = FP.takeFileName $ FSN.eventPath event

    watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan dirInWaspProjectDir =
      FSN.watchTreeChan mgr (SP.fromAbsDir $ waspProjectDir </> dirInWaspProjectDir) eventFilter chan
      where
        eventFilter = isWatchedFile . FP.takeFileName . FSN.eventPath

    -- TODO: Might be valuable to also filter out files from .gitignore.
    isWatchedFile :: String -> Bool
    isWatchedFile filename =
      not (isEditorTmpFile filename)
        && filename /= ".DS_Store"

    listenForEvents :: Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents chan lastCompileTime = do
      event <- readChan chan
      if isStaleEvent event lastCompileTime
        then -- Ignore delayed/stale events older than our last compile time.
          listenForEvents chan lastCompileTime
        else do
          -- Recompile, but only after a 1s period of no new events.
          changeBatch <- collectChangeBatchUntilQuiet chan lastCompileTime 1 event
          currentTime <- getCurrentTime
          (warnings, errors) <- recompile changeBatch
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

    -- Collects events until no new events are received for a duration of `secondsToDelay`.
    -- If a stale event arrives during an active timer window, we immediately return control to the caller.
    collectChangeBatchUntilQuiet :: Chan FSN.Event -> UTCTime -> Int -> FSN.Event -> IO ChangeBatch
    collectChangeBatchUntilQuiet chan lastCompileTime secondsToDelay firstEvent =
      collectEvents [firstEvent]
      where
        collectEvents events = do
          eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
          case eventOrDelay of
            Left event
              | isStaleEvent event lastCompileTime -> return $ ChangeBatch $ reverse events
              | otherwise ->
                  -- We have a new event, restart waiting process.
                  collectEvents $ event : events
            Right () -> return $ ChangeBatch $ reverse events

    isStaleEvent :: FSN.Event -> UTCTime -> Bool
    isStaleEvent event lastCompileTime = FSN.eventTime event < lastCompileTime
    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    recompile :: ChangeBatch -> IO ([CompileWarning], [CompileError])
    recompile changeBatch = do
      cliSendMessage $ Msg.Start "Recompiling on file change..."
      (warnings, errors) <- compileIO waspProjectDir outDir

      printCompilationResult (warnings, errors)
      if null errors
        then do
          let serverChangeImpact = classifyServerChangeImpact changeBatch
          _onSuccessfulCompile watchCompileHooks serverChangeImpact
        else
          cliSendMessage (Msg.Failure "Recompilation on file change failed." $ show (length errors) ++ " errors found")
            >> _onFailedCompile watchCompileHooks

      return (warnings, errors)

    classifyServerChangeImpact :: ChangeBatch -> ServerChangeImpact
    classifyServerChangeImpact (ChangeBatch events)
      | any eventMightAffectServer events = ServerMightBeAffected
      | otherwise = ServerUnaffected

    eventMightAffectServer :: FSN.Event -> Bool
    eventMightAffectServer event =
      topLevelFileMightAffectServer pathInProject
        || srcFileMightAffectServer pathInProject
      where
        pathInProject = FP.normalise $ FP.makeRelative (SP.fromAbsDir waspProjectDir) (FSN.eventPath event)

    topLevelFileMightAffectServer :: FilePath -> Bool
    topLevelFileMightAffectServer pathInProject =
      case FP.splitDirectories pathInProject of
        [filename] -> filename `elem` serverRelevantTopLevelFiles
        _ -> False

    srcFileMightAffectServer :: FilePath -> Bool
    srcFileMightAffectServer pathInProject =
      case FP.splitDirectories pathInProject of
        srcDirName : _ -> srcDirName == FP.dropTrailingPathSeparator (SP.fromRelDir srcDirInWaspProjectDir) && FP.takeExtension pathInProject `elem` serverRelevantExtensions
        _ -> False

    serverRelevantTopLevelFiles :: [FilePath]
    serverRelevantTopLevelFiles =
      [ "main.wasp",
        "main.wasp.ts",
        ".env",
        ".env.server",
        "schema.prisma",
        "package.json",
        ".waspignore"
      ]

    serverRelevantExtensions :: [String]
    serverRelevantExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]

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
