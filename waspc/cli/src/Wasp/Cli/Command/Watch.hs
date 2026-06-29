module Wasp.Cli.Command.Watch
  ( watch,
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
import Wasp.Generator (ServerUpdateImpact (..))
import qualified Wasp.Generator.Common as GeneratorCommon
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import Wasp.Project.Common (srcDirInWaspProjectDir)

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
-- After successful recompilation, it refreshes the server if the change may affect it.
-- After failed recompilation, it stops the server.
watch ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratorCommon.GeneratedAppDir) ->
  MVar ([CompileWarning], [CompileError]) ->
  (ServerUpdateImpact -> IO ()) ->
  IO () ->
  IO ()
watch waspProjectDir outDir ongoingCompilationResultMVar updateServer stopServer = FSN.withManager $ \mgr -> do
  chan <- newChan
  _ <- watchFilesAtTopLevelOfWaspProjectDir mgr chan
  _ <- watchFilesAtAllLevelsOfDirInWaspProjectDir mgr chan srcDirInWaspProjectDir
  startListeningTime <- getCurrentTime
  listenForEvents chan startListeningTime
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
          sourceChangeEvents <- collectEventsUntilQuiet chan lastCompileTime 1 [event]
          currentTime <- getCurrentTime
          (warnings, errors) <- recompile
          updateOngoingCompilationResultMVar (warnings, errors)
          handleRecompileResult sourceChangeEvents errors
          listenForEvents chan currentTime

    handleRecompileResult :: [FSN.Event] -> [CompileError] -> IO ()
    handleRecompileResult sourceChangeEvents errors =
      if null errors
        then do
          let updateImpact = serverUpdateImpact sourceChangeEvents
          case updateImpact of
            ServerMayBeAffected -> cliSendMessage $ Msg.Start "Updating server..."
            ServerUnaffected -> return ()
          updateServer updateImpact
        else do
          cliSendMessage $
            Msg.Failure "Recompilation on file change failed." $
              show (length errors) ++ " errors found"
          stopServer

    serverUpdateImpact :: [FSN.Event] -> ServerUpdateImpact
    serverUpdateImpact sourceChangeEvents =
      if any serverMayBeAffectedBy sourceChangeEvents
        then ServerMayBeAffected
        else ServerUnaffected

    serverMayBeAffectedBy :: FSN.Event -> Bool
    serverMayBeAffectedBy event =
      case FP.splitDirectories $ FP.makeRelative (SP.fromAbsDir waspProjectDir) (FSN.eventPath event) of
        [] -> False
        [srcDir]
          | srcDir == srcDirName -> False
        srcDir : _
          | srcDir == srcDirName -> FP.takeExtension (FSN.eventPath event) `elem` serverSourceExtensions
        [filename] -> topLevelFileMayAffectServer filename
        _ -> False

    srcDirName :: FilePath
    srcDirName = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir

    topLevelFileMayAffectServer :: FilePath -> Bool
    topLevelFileMayAffectServer filename =
      filename `elem` serverTopLevelFiles
        || FP.takeExtension filename `elem` serverSourceExtensions

    serverTopLevelFiles :: [FilePath]
    serverTopLevelFiles =
      [ "main.wasp",
        "main.wasp.ts",
        ".env",
        ".env.server",
        "schema.prisma",
        "package.json",
        "tsconfig.json",
        "tsconfig.src.json",
        "tsconfig.wasp.json",
        ".waspignore"
      ]

    serverSourceExtensions :: [String]
    serverSourceExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]

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
    collectEventsUntilQuiet :: Chan FSN.Event -> UTCTime -> Int -> [FSN.Event] -> IO [FSN.Event]
    collectEventsUntilQuiet chan lastCompileTime secondsToDelay events = do
      eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
      case eventOrDelay of
        Left event ->
          if isStaleEvent event lastCompileTime
            then return $ reverse events
            else -- We have a new event, restart waiting process.
              collectEventsUntilQuiet chan lastCompileTime secondsToDelay $ event : events
        Right () -> return $ reverse events

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
