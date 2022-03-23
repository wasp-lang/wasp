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
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Message as Msg

clean :: Command ()
clean = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let dotWaspDirFp = SP.toFilePath $ waspProjectDir SP.</> Common.dotWaspDirInWaspProjectDir
  cliSendMessageC $ Msg.Start "Deleting .wasp/ directory..."
  doesDotWaspDirExist <- liftIO $ doesDirectoryExist dotWaspDirFp
  if doesDotWaspDirExist
    then do
      liftIO $ removeDirectoryRecursive dotWaspDirFp
      cliSendMessageC $ Msg.Success "Deleted .wasp/ directory."
    else cliSendMessageC $ Msg.Success "Nothing to delete: .wasp directory does not exist."
