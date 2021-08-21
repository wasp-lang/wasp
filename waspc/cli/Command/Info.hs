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
import StrongPath (Abs, Dir, File', Path', toFilePath)
import StrongPath.Operations
import System.Directory (doesFileExist, getFileSize)
import System.Directory.Recursive (getDirRecursive)
import qualified Util.Terminal as Term
import Wasp (Wasp, appName, getApp)

info :: Command ()
info =
  do
    waspDir <- findWaspProjectRootDirFromCwd
    let dotWaspInfoFile = waspDir </> Cli.Common.dotWaspDirInWaspProjectDir </> Cli.Common.generatedCodeDirInDotWaspDir </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir
    waspAstOrError <- liftIO $ parseWaspFile waspDir
    case eitherStringWasp of
      Left err -> waspSaysC err
      Right wasp -> do
        compileInfo <- liftIO $ readDotWaspInfoFile dotWaspInfoFile
        projectSize <- liftIO $ getDirectorySize $ toFilePath waspDir
        waspSaysC $
            unlines
              [ "",
                title "Project information",
                printInfo
                  "Name"
                  (appName $ getApp wasp),
                printInfo
                  "Compilation"
                  compileInfo,
                printInfo
                  "Project size"
                  (show $ projectSize `div` 1000)
                  ++ " "
                  ++ "KB"
              ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

getDirectorySize :: FilePath -> IO Integer
getDirectorySize path = sum <$> (getDirRecursive path >>= mapM getFileSize)

readDotWaspInfoFile :: Path' Abs File' -> IO String
readDotWaspInfoFile path = do
  dotWaspInfoFileExists <- doesFileExist $ toFilePath path
  if dotWaspInfoFileExists
    then do readFile (toFilePath path)
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
