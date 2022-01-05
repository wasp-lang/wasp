module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import System.Directory
  ( doesDirectoryExist,
    removeDirectoryRecursive,
  )
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspStartMessage, asWaspSuccessMessage)

clean :: Command ()
clean = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let dotWaspDirFp = SP.toFilePath $ waspProjectDir SP.</> Common.dotWaspDirInWaspProjectDir
  waspSaysC $ asWaspStartMessage "Deleting .wasp/ directory..."
  doesDotWaspDirExist <- liftIO $ doesDirectoryExist dotWaspDirFp
  if doesDotWaspDirExist
    then do
      liftIO $ removeDirectoryRecursive dotWaspDirFp
      waspSaysC $ asWaspSuccessMessage "Deleted .wasp/ directory."
    else waspSaysC "Nothing to delete: .wasp directory does not exist."
