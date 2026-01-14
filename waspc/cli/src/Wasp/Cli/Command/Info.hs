module Wasp.Cli.Command.Info
  ( info,
  )
where

import Control.Arrow ()
import Control.Monad.IO.Class (MonadIO (liftIO))
import StrongPath (Abs, Dir, Path', fromRelFile, (</>))
import StrongPath.Operations ()
import System.Directory (getFileSize)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Terminal (title)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

info :: Command ()
info = do
  InWaspProject waspDir <- require

  projectSize <- liftIO $ readDirectorySizeMB waspDir

  let generatedCodeDir =
        waspDir
          </> Project.Common.dotWaspDirInWaspProjectDir
          </> Project.Common.generatedCodeDirInDotWaspDir
  compileInfo <- liftIO $ WaspInfo.safeRead generatedCodeDir
  let compileInfoStr = maybe "Not compiled yet" WaspInfo.showWaspInfo compileInfo

  appSpec <- analyze waspDir
  let (appName, _) = ASV.getApp appSpec

  cliSendMessageC $
    Msg.Info $
      unlines
        [ "",
          title "Project information",
          printInfo "Name" appName,
          printInfo "Database system" $ show $ ASV.getValidDbSystem appSpec,
          printInfo "Last compile" compileInfoStr,
          printInfo "Project dir size" projectSize
        ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> allFileSizes
  where
    allFileSizes = IOUtil.listDirectoryDeep path >>= mapM (getFileSize . fromRelFile)
