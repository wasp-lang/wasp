module Command.Watch
  ( watch,
  )
where

import Cli.Common (waspSays)
import qualified Cli.Common as Common
import Command.Compile (compileIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (unless, when)
import Data.List (isSuffixOf)
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
  )
import qualified Lib
import StrongPath (Abs, Dir, Path, (</>))
import qualified StrongPath as SP
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP

-- TODO: Another possible problem: on re-generation, wasp re-generates a lot of files, even those that should not
--   be generated again, since it is not smart enough yet to know which files do not need to be regenerated.
--   This can trigger `npm start` processes to reload multiple times, once for each file!
--   `nodemon` specifically has --delay option which says how long it should wait before restarting,
--   and it's default value is 1 second, so it will restart only once if all file changes happen in one second interval.
--   We could play in the future with increasing this delay. Nodemon can also be manually restarted with `rs` so
--   that could also be useful -> if we could do only manual restarting and not have it restart on its own, we could
--   have tigther control over it. But do we need nodemon at all then hm :)?
-- TODO: Idea: Read .gitignore file, and ignore everything from it. This will then also cover the
--   .wasp dir, and users can easily add any custom stuff they want ignored. But, we also have to
--   be ready for the case when there is no .gitignore, that could be possible.

-- | Forever listens for any file changes in waspProjectDir, and if there is a change,
--   compiles Wasp source files in waspProjectDir and regenerates files in outDir.
watch :: Path Abs (Dir Common.WaspProjectDir) -> Path Abs (Dir Lib.ProjectRootDir) -> IO ()
watch waspProjectDir outDir = FSN.withManager $ \mgr -> do
  currentTime <- getCurrentTime
  chan <- newChan
  _ <- FSN.watchDirChan mgr (SP.toFilePath waspProjectDir) eventFilter chan
  _ <- FSN.watchTreeChan mgr (SP.toFilePath $ waspProjectDir </> Common.extCodeDirInWaspProjectDir) eventFilter chan
  timeOfLastEvent <- MVar.newMVar currentTime
  listenForEvents chan currentTime timeOfLastEvent
  where
    oneSecond :: Int
    oneSecond = 1000000

    oneSecondDelay :: MVar.MVar UTCTime -> IO ()
    oneSecondDelay timeOfLastEventMVar = do
      threadDelay oneSecond
      currentDelayTime <- MVar.readMVar timeOfLastEventMVar
      currentTime <- getCurrentTime
      let timeDiff = nominalDiffTimeToSeconds $ diffUTCTime currentTime currentDelayTime
      when (timeDiff < fromIntegral oneSecond) $ do
        threadDelay (oneSecond - (floor . (* 1e9) $ timeDiff))
        oneSecondDelay timeOfLastEventMVar

    recurEventCheck :: Chan FSN.Event -> UTCTime -> MVar.MVar UTCTime -> IO ()
    recurEventCheck chan currentTime timeOfLastEventMVar = do
      event <- readChan chan
      let eventTime = FSN.eventTime event
      unless (eventTime < currentTime) $ do
        MVar.putMVar timeOfLastEventMVar eventTime

    listenForEvents :: Chan FSN.Event -> UTCTime -> MVar.MVar UTCTime -> IO ()
    listenForEvents chan lastCompileTime timeOfLastEventMVar = do
      event <- readChan chan
      let eventTime = FSN.eventTime event
      if eventTime < lastCompileTime
        then do
          -- If event happened before last compilation started, skip it.
          MVar.putMVar timeOfLastEventMVar lastCompileTime
          listenForEvents chan lastCompileTime timeOfLastEventMVar
        else do
          currentTime <- getCurrentTime
          MVar.putMVar timeOfLastEventMVar currentTime
          _ <- race (recurEventCheck chan currentTime timeOfLastEventMVar) (oneSecondDelay timeOfLastEventMVar)
          -- These will be executed only after race is completed
          recompile
          listenForEvents chan currentTime timeOfLastEventMVar

    recompile :: IO ()
    recompile = do
      waspSays "Recompiling on file change..."
      compilationResult <- compileIO waspProjectDir outDir
      case compilationResult of
        Left err -> waspSays $ "Recompilation on file change failed: " ++ err
        Right () -> waspSays "Recompilation on file change succeeded."
      return ()

    -- TODO: This is a hardcoded approach to ignoring most of the common tmp files that editors
    --   create next to the source code. Bad thing here is that users can't modify this,
    --   so better approach would be probably to use information from .gitignore instead, or
    --   maybe combining the two somehow.
    eventFilter :: FSN.Event -> Bool
    eventFilter event =
      let filename = FP.takeFileName $ FSN.eventPath event
       in not (null filename)
            && not (take 2 filename == ".#") -- Ignore emacs lock files.
            && not (head filename == '#' && last filename == '#') -- Ignore emacs auto-save files.
            && not (last filename == '~') -- Ignore emacs and vim backup files.
            && not (head filename == '.' && ".swp" `isSuffixOf` filename) -- Ignore vim swp files.
            && not (head filename == '.' && ".un~" `isSuffixOf` filename) -- Ignore vim undo files.
