{-# LANGUAGE TypeApplications #-}

module Wasp.Cli.Command.Info
  ( info,
  )
where

import Control.Arrow (ArrowChoice (left))
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', fromAbsFile, fromRelFile, toFilePath)
import StrongPath.Operations
import System.Directory (doesFileExist, getFileSize)
import qualified Wasp.Analyzer as Analyzer
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Cli.Terminal (title)
import Wasp.Common (WaspProjectDir)
import Wasp.Error (showCompilerErrorForTerminal)
import Wasp.Lib (findWaspFile)
import Wasp.Util.IO (listDirectoryDeep)
import qualified Wasp.Util.Terminal as Term

info :: Command ()
info =
  do
    waspDir <- findWaspProjectRootDirFromCwd
    compileInfo <- liftIO $ readCompileInformation waspDir
    projectSize <- liftIO $ readDirectorySizeMB waspDir
    declsOrError <- liftIO $ parseWaspFile waspDir
    case declsOrError of
      Left err -> waspSaysC err
      Right decls -> do
        waspSaysC $
          unlines
            [ "",
              title "Project information",
              printInfo
                "Name"
                (fst $ head $ AS.takeDecls @AS.App.App decls),
              printInfo
                "Last compile"
                compileInfo,
              printInfo
                "Project size"
                projectSize
            ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> (listDirectoryDeep path >>= mapM (getFileSize . fromRelFile))

readCompileInformation :: Path' Abs (Dir WaspProjectDir) -> IO String
readCompileInformation waspDir = do
  let dotWaspInfoFile =
        fromAbsFile $
          waspDir </> Cli.Common.dotWaspDirInWaspProjectDir
            </> Cli.Common.generatedCodeDirInDotWaspDir
            </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir
  dotWaspInfoFileExists <- doesFileExist dotWaspInfoFile
  if dotWaspInfoFileExists
    then do readFile dotWaspInfoFile
    else return "No compile information found"

parseWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String [AS.Decl])
parseWaspFile waspDir = do
  maybeWaspFile <- findWaspFile waspDir
  case maybeWaspFile of
    Nothing -> return (Left "Couldn't find a single *.wasp file.")
    Just waspFile ->
      do
        waspStr <- readFile (toFilePath waspFile)
        return $
          left
            ( ("Couldn't parse .wasp file:\n" <>)
                . showCompilerErrorForTerminal (waspFile, waspStr)
                . Analyzer.getErrorMessageAndCtx
            )
            $ Analyzer.analyze waspStr
