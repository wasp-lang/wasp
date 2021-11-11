module Wasp.Cli.Command.Watch
  ( watch,
  )
where

import Control.Concurrent.Chan (Chan, newChan, readChan)
import Data.List (isSuffixOf)
import Data.Time.Clock (UTCTime, getCurrentTime)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import qualified System.FSNotify as FSN
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (compileIO)
import Wasp.Cli.Common (waspSays)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib

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
watch :: Path' Abs (Dir Common.WaspProjectDir) -> Path' Abs (Dir Wasp.Lib.ProjectRootDir) -> IO ()
watch waspProjectDir outDir = FSN.withManager $ \mgr -> do
  currentTime <- getCurrentTime
  chan <- newChan
  _ <- FSN.watchDirChan mgr (SP.fromAbsDir waspProjectDir) eventFilter chan
  _ <- FSN.watchTreeChan mgr (SP.fromAbsDir $ waspProjectDir </> Common.extCodeDirInWaspProjectDir) eventFilter chan
  listenForEvents chan currentTime
  where
    listenForEvents :: Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents chan lastCompileTime = do
      event <- readChan chan
      let eventTime = FSN.eventTime event
      if eventTime < lastCompileTime
        then -- If event happened before last compilation started, skip it.
          listenForEvents chan lastCompileTime
        else do
          currentTime <- getCurrentTime
          recompile
          listenForEvents chan currentTime

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
