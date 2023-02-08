{-# LANGUAGE TypeApplications #-}

module Wasp.Cli.Command.Info
  ( info,
  )
where

import Control.Arrow
import Control.Monad.Except
import StrongPath (Abs, Dir, Path', fromRelFile)
import StrongPath.Operations
import System.Directory (getFileSize)
import qualified Wasp.Analyzer as Analyzer
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Core.Decl as AS (Decl, takeDecls)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Cli.Terminal (title)
import Wasp.Common (WaspProjectDir)
import Wasp.Error (showCompilerErrorForTerminal)
import Wasp.Lib (findWaspFile)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

info :: Command ()
info = do
  waspDir <- findWaspProjectRootDirFromCwd
  compileInfo <- liftIO $ readCompileInformation waspDir
  projectSize <- liftIO $ readDirectorySizeMB waspDir
  declsOrError <- liftIO $ parseWaspFile waspDir
  case declsOrError of
    Left err -> cliSendMessageC $ Msg.Failure "Info failed" err
    Right decls -> do
      cliSendMessageC $
        Msg.Info $
          unlines
            [ "",
              title "Project information",
              printInfo "Name" (fst $ head $ AS.takeDecls @AS.App.App decls),
              printInfo "Last compile" compileInfo,
              printInfo "Project size" projectSize
            ]

printInfo :: String -> String -> String
printInfo key value = Term.applyStyles [Term.Cyan] key ++ ": " <> Term.applyStyles [Term.White] value

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> allFileSizes
  where
    allFileSizes = IOUtil.listDirectoryDeep path >>= mapM (getFileSize . fromRelFile)

readCompileInformation :: Path' Abs (Dir WaspProjectDir) -> IO String
readCompileInformation waspDir = do
  let dotWaspInfoFile =
        waspDir </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir
          </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir
  dotWaspInfoFileExists <- IOUtil.doesFileExist dotWaspInfoFile
  if dotWaspInfoFileExists
    then do IOUtil.readFile dotWaspInfoFile
    else return "No compile information found"

parseWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String [AS.Decl])
parseWaspFile waspDir = runExceptT $ do
  waspFile <- ExceptT $ findWaspFile waspDir
  waspStr <- liftIO $ IOUtil.readFile waspFile
  liftEither $ left (annotateErrorForCli waspFile waspStr) $ Analyzer.analyze waspStr
  where
    annotateErrorForCli waspFile waspStr =
      ("Couldn't parse .wasp file:\n" ++)
        . showCompilerErrorForTerminal (waspFile, waspStr)
        . Analyzer.getErrorMessageAndCtx
