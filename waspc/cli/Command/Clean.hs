module Command.Clean
  ( clean,
  )
where

import qualified Cli.Common as Common
import Command (Command)
import Command.Common (findWaspProjectRootDirFromCwdCmd)
import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import System.Directory
  ( doesDirectoryExist,
    removeDirectoryRecursive,
  )
import System.IO (hFlush, stdout)

clean :: Command ()
clean = do
  waspProjectDir <- findWaspProjectRootDirFromCwdCmd
  let dotWaspDirFp = SP.toFilePath $ waspProjectDir SP.</> Common.dotWaspDirInWaspProjectDir
  liftIO $ putStrLn "Deleting .wasp/ directory..." >> hFlush stdout
  doesDotWaspDirExist <- liftIO $ doesDirectoryExist dotWaspDirFp
  if doesDotWaspDirExist
    then liftIO $ do
      removeDirectoryRecursive dotWaspDirFp
      putStrLn "Deleted .wasp/ directory."
    else liftIO $ putStrLn "Nothing to delete: .wasp directory does not exist."
