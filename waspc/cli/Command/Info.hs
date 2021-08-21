module Command.Info
  ( info,
  )
where

import qualified Cli.Common
import Cli.Terminal (title)
import Command (Command)
import Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import Common (WaspProjectDir)
import Control.Monad.IO.Class (liftIO)
import Lib (findWaspFile)
import qualified Parser
import qualified Path as P
import StrongPath (Abs, Dir, Path', toFilePath)
import StrongPath.Operations
import StrongPath.Path (toPathAbsDir)
import System.Directory (doesFileExist, getFileSize)
import Util.IO (listDirectoryDeep)
import qualified Util.Terminal as Term
import Wasp (Wasp, appName, getApp)

info :: Command ()
info =
  do
    waspDir <- findWaspProjectRootDirFromCwd
    waspAstOrError <- liftIO $ parseWaspFile waspDir
    case waspAstOrError of
      Left err -> waspSaysC err
      Right wasp -> do
        compileInfo <- liftIO $ readCompileInformation waspDir
        projectSize <- liftIO $ readDirectorySizeMB waspDir
        waspSaysC $
          unlines
            [ "",
              title "Project information",
              printInfo
                "Name"
                (appName $ getApp wasp),
              printInfo
                "Last compilation"
                compileInfo,
              printInfo
                "Project size"
                projectSize
            ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> (listDirectoryDeep (toPathAbsDir path) >>= mapM (getFileSize . P.toFilePath))

readCompileInformation :: Path' Abs (Dir WaspProjectDir) -> IO String
readCompileInformation waspDir = do
  let dotWaspInfoFile = toFilePath $ waspDir </> Cli.Common.dotWaspDirInWaspProjectDir </> Cli.Common.generatedCodeDirInDotWaspDir </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir
  dotWaspInfoFileExists <- doesFileExist dotWaspInfoFile
  if dotWaspInfoFileExists
    then do readFile dotWaspInfoFile
    else return "No compile information found"

parseWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String Wasp)
parseWaspFile waspDir = do
  maybeWaspFile <- findWaspFile waspDir
  case maybeWaspFile of
    Nothing -> return (Left "Couldn't find a single *.wasp file.")
    Just waspFile -> do
      waspStr <- readFile (toFilePath waspFile)
      case Parser.parseWasp waspStr of
        Left err -> return (Left ("Couldn't parse .wasp file: " <> show err))
        Right wasp -> return (Right wasp)
