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
import StrongPath (Abs, Dir, File', Path', relfile, toFilePath)
import StrongPath.Operations
import System.Directory (doesFileExist, getFileSize)
import System.Directory.Recursive (getDirRecursive)
import qualified Util.Terminal as Term
import Wasp (Wasp, appName, getApp)

info :: Command ()
info =
  do
    waspDir <- findWaspProjectRootDirFromCwd
    let dotWaspInfoFile = waspDir </> Cli.Common.dotWaspDirInWaspProjectDir </> Cli.Common.generatedCodeDirInDotWaspDir </> [relfile|.waspinfo|]
    maybeWasp <- liftIO $ parseWaspFile waspDir
    case maybeWasp of
      Left err -> waspSaysC err
      Right wasp -> do
        buildInfo <- liftIO $ readDotWaspInfoFile dotWaspInfoFile
        projectSize <- liftIO $ getDirectorySize $ toFilePath waspDir
        liftIO $
          putStrLn $
            unlines
              [ "",
                title "Project information",
                printInfo
                  "Name"
                  (appName $ getApp wasp),
                printInfo
                  "Last build"
                  buildInfo,
                printInfo
                  "Project size"
                  (show $ projectSize `div` 1000)
                  ++ " "
                  ++ "KB"
              ]

getDirectorySize :: FilePath -> IO Integer
getDirectorySize path = sum <$> (getDirRecursive path >>= mapM getFileSize)

readDotWaspInfoFile :: Path' Abs File' -> IO String
readDotWaspInfoFile path = do
  dotWaspInfoFileExists <- liftIO $ doesFileExist $ toFilePath path
  if dotWaspInfoFileExists
    then do readFile (toFilePath path)
    else return "No builds yet"

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

parseWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String Wasp)
parseWaspFile waspDir = do
  maybeWaspFile <- findWaspFile waspDir
  case maybeWaspFile of
    Nothing -> return (Left "Couldn't find a single *.wasp file.")
    Just waspFile -> do
      waspStr <- liftIO $ readFile (toFilePath waspFile)

      case Parser.parseWasp waspStr of
        Left err -> return (Left ("Couldn't parse .wasp file: " <> show err))
        Right wasp -> return (Right wasp)
