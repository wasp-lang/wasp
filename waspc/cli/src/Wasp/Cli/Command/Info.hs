module Wasp.Cli.Command.Info
  ( info,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadIO (liftIO))
import StrongPath (Abs, Dir, Path', fromRelFile)
import StrongPath.Operations ()
import System.Directory (getFileSize)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (readWaspCompileInfo)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Start.Db (getDbSystem)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

info :: Command ()
info = do
  InWaspProject waspDir <- require

  compileInfo <- liftIO $ readWaspCompileInfo waspDir
  projectSize <- liftIO $ readDirectorySizeMB waspDir

  appSpec <- analyze waspDir
  let (appName, app) = ASV.getApp appSpec

  cliSendMessageC $
    Msg.Info $
      unlines
        [ "",
          title "Project information",
          printInfo "Name" appName,
          printInfo "Database system" $ show $ getDbSystem app,
          printInfo "Last compile" compileInfo,
          printInfo "Project dir size" projectSize
        ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> allFileSizes
  where
    allFileSizes = IOUtil.listDirectoryDeep path >>= mapM (getFileSize . fromRelFile)
