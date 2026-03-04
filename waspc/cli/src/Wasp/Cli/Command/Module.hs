module Wasp.Cli.Command.Module
  ( moduleBuild,
    moduleBuildWatch,
    moduleInit,
  )
where

import Control.Concurrent (newChan, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (withObject, (.:))
import Data.Char (toUpper)
import Data.List (foldl', isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Path', castFile, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, makeAbsolute)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.FSNotify as FSN
import Wasp.AppSpec.Module (ModuleSpec (msEntities))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Data as Data
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInProjectRootDir)
import Wasp.Generator.FileDraft (Writeable (write))
import Wasp.Generator.ModuleGenerator (genModuleSdk)
import Wasp.Generator.Valid.PackageJson.ModuleDependencies (moduleDependenciesValidator)
import qualified Wasp.Generator.Valid.Validator as V
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import Wasp.Project.WaspFile.WaspConfigPackage (ensureWaspConfigPackageInstalled)
import qualified Wasp.Util.IO as IOUtil

newtype ModuleSpecEnvelope = ModuleSpecEnvelope
  { _envSpec :: ModuleSpec
  }

instance Aeson.FromJSON ModuleSpecEnvelope where
  parseJSON = withObject "ModuleSpecEnvelope" $ \v ->
    ModuleSpecEnvelope <$> v .: "spec"

moduleBuild :: Command ()
moduleBuild = do
  InWaspProject waspProjectDir <- require
  cliSendMessageC $ Msg.Start "Building module SDK..."
  result <- liftIO $ runModuleBuild waspProjectDir
  case result of
    Left err -> throwError $ CommandError "Module build failed" err
    Right () -> return ()

moduleBuildWatch :: Command ()
moduleBuildWatch = do
  InWaspProject waspProjectDir <- require
  cliSendMessageC $ Msg.Start "Building module SDK (watch mode)..."

  result <- liftIO $ runModuleBuild waspProjectDir
  case result of
    Left err -> throwError $ CommandError "Module build failed" err
    Right () -> return ()

  cliSendMessageC $ Msg.Info "Watching for file changes..."
  liftIO $ watchLoop waspProjectDir

-- | The single build pipeline implementation. Returns Left on failure.
runModuleBuild :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
runModuleBuild waspProjectDir = runExceptT $ do
  let moduleWaspTsFile = waspProjectDir </> [relfile|module.wasp.ts|]
  moduleWaspTsExists <- liftIO $ IOUtil.doesFileExist moduleWaspTsFile
  unless moduleWaspTsExists $
    ExceptT . return . Left $ "Could not find module.wasp.ts in the project directory."

  liftIO $ cliSendMessage $ Msg.Info "Validating package.json dependencies..."
  pkgJson <- ExceptT $ prefixLeft "Failed to read package.json: " <$> readPackageJsonFile waspProjectDir
  let validationErrors = V.execValidator moduleDependenciesValidator pkgJson
  unless (null validationErrors) $
    ExceptT . return . Left $
      "Module package.json has invalid dependencies:\n"
        ++ unlines (map show validationErrors)

  -- Ensure the SDK stub package.json exists so npm can resolve the
  -- workspace symlink on the very first `npm install`.
  let sdkDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> [reldir|sdk/wasp|]
  let stubPkgJsonPath = fromAbsFile $ sdkDir </> [relfile|package.json|]
  liftIO $ do
    sdkDirExists <- doesDirectoryExist (fromAbsDir sdkDir)
    unless sdkDirExists $ do
      createDirectoryIfMissing True (fromAbsDir sdkDir)
      writeFile stubPkgJsonPath stubSdkPackageJson

  liftIO $ cliSendMessage $ Msg.Info "Ensuring wasp-config package is installed..."
  ExceptT $ prefixLeft "Failed to install wasp-config: " <$> ensureWaspConfigPackageInstalled waspProjectDir

  liftIO $ cliSendMessage $ Msg.Info "Compiling module.wasp.ts..."
  runStep
    "TypeScript compilation of module.wasp.ts failed."
    "npx"
    [ "tsc",
      "-p",
      fromAbsFile (waspProjectDir </> [relfile|tsconfig.wasp.json|]),
      "--noEmit",
      "false",
      "--outDir",
      fromAbsDir (waspProjectDir </> dotWaspDirInWaspProjectDir)
    ]

  let compiledJsFile = waspProjectDir </> dotWaspDirInWaspProjectDir </> castFile [relfile|module.wasp.js|]
  let outputFile = waspProjectDir </> dotWaspDirInWaspProjectDir </> castFile [relfile|decls.json|]

  liftIO $ cliSendMessage $ Msg.Info "Evaluating module spec..."
  runStep
    "Error while evaluating module.wasp.ts."
    "npx"
    [ "wasp-config",
      fromAbsFile compiledJsFile,
      fromAbsFile outputFile,
      "[]"
    ]

  liftIO $ cliSendMessage $ Msg.Info "Parsing module spec..."
  jsonBytes <- liftIO $ IOUtil.readFileBytes outputFile
  envelope <- ExceptT . return $ prefixLeft "Failed to parse module spec JSON: " $ Aeson.eitherDecode jsonBytes
  let spec = _envSpec envelope

  liftIO $ cliSendMessage $ Msg.Info "Generating module SDK..."
  let fileDrafts = genModuleSdk spec
  let outDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir
  liftIO $ do
    createDirectoryIfMissing True (fromAbsDir outDir)
    mapM_ (write outDir) fileDrafts

  unless (null $ msEntities spec) $ do
    liftIO $ cliSendMessage $ Msg.Info "Running prisma generate..."
    let schemaPath = outDir </> dbSchemaFileInProjectRootDir
    runStep
      "prisma generate failed."
      "npx"
      [ "prisma",
        "generate",
        "--schema=" ++ fromAbsFile (castFile schemaPath)
      ]

  liftIO $ cliSendMessage $ Msg.Info "Verifying types with tsc --noEmit..."
  runStep "tsc --noEmit found type errors." "npx" ["tsc", "--noEmit"]

  liftIO $ cliSendMessage $ Msg.Success "Module SDK generated successfully."
  where
    runStep errMsg cmd args = do
      exitCode <- liftIO $ runJobAndWait waspProjectDir cmd args
      case exitCode of
        ExitFailure _ -> ExceptT . return $ Left errMsg
        ExitSuccess -> return ()

    prefixLeft prefix = either (Left . (prefix ++)) Right

stubSdkPackageJson :: String
stubSdkPackageJson = "{\"name\":\"wasp\",\"version\":\"0.0.0\",\"type\":\"module\"}"

runJobAndWait :: Path' Abs (Dir WaspProjectDir) -> String -> [String] -> IO ExitCode
runJobAndWait waspProjectDir cmd args = do
  chan <- newChan
  (_, exitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (runNodeCommandAsJob waspProjectDir cmd args J.Wasp chan)
  return exitCode

watchLoop :: Path' Abs (Dir WaspProjectDir) -> IO ()
watchLoop waspProjectDir = FSN.withManager $ \mgr -> do
  chan <- newChan
  let srcDir = waspProjectDir </> srcDirInWaspProjectDir
  _ <- FSN.watchDirChan mgr (SP.fromAbsDir waspProjectDir) topLevelFilter chan
  _ <- FSN.watchTreeChan mgr (SP.fromAbsDir srcDir) srcFilter chan
  cliSendMessage $ Msg.Info "Watching module.wasp.ts and src/ for changes..."
  listenForEvents chan =<< getCurrentTime
  where
    topLevelFilter event =
      let filename = FP.takeFileName (FSN.eventPath event)
       in isWatchedFile filename && filename /= "package-lock.json"

    srcFilter = isWatchedFile . FP.takeFileName . FSN.eventPath

    isWatchedFile :: String -> Bool
    isWatchedFile filename =
      not (isEditorTmpFile filename)
        && filename /= ".DS_Store"

    listenForEvents :: Chan FSN.Event -> UTCTime -> IO ()
    listenForEvents chan lastBuildTime = do
      event <- readChan chan
      if FSN.eventTime event < lastBuildTime
        then listenForEvents chan lastBuildTime
        else do
          waitUntilNoNewEvents chan lastBuildTime 1
          currentTime <- getCurrentTime
          rebuild
          listenForEvents chan currentTime

    waitUntilNoNewEvents :: Chan FSN.Event -> UTCTime -> Int -> IO ()
    waitUntilNoNewEvents chan lastBuildTime secondsToDelay = do
      eventOrDelay <- race (readChan chan) (threadDelaySeconds secondsToDelay)
      case eventOrDelay of
        Left event ->
          unless (FSN.eventTime event < lastBuildTime) $
            waitUntilNoNewEvents chan lastBuildTime secondsToDelay
        Right () -> return ()

    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    rebuild :: IO ()
    rebuild = do
      cliSendMessage $ Msg.Start "Rebuilding module SDK..."
      result <- runModuleBuild waspProjectDir
      case result of
        Left err -> cliSendMessage $ Msg.Failure "Module rebuild failed." err
        Right () -> return ()

    isEditorTmpFile :: String -> Bool
    isEditorTmpFile filename = case filename of
      [] -> False
      (c : _) ->
        or
          [ ".#" `isPrefixOf` filename,
            c == '#' && "#" `isSuffixOf` filename,
            "~" `isSuffixOf` filename,
            c == '.' && ".swp" `isSuffixOf` filename
          ]

moduleInit :: String -> Command ()
moduleInit name = do
  let camelName = "create" ++ toCamelCase name
      dirName = FP.takeFileName name
  cliSendMessageC $ Msg.Start $ "Initializing module '" ++ name ++ "'..."

  dirExists <- liftIO $ doesDirectoryExist dirName
  if dirExists
    then
      throwError $
        CommandError "Module init failed" $
          "Directory '" ++ dirName ++ "' already exists."
    else do
      absProjectDir <- liftIO $ SP.parseAbsDir =<< makeAbsolute dirName
      liftIO $ do
        dataDir <- Data.getAbsDataDirPath
        let templateDir = dataDir </> [reldir|Cli/module-template|]
        copyDirRecur (toPathAbsDir templateDir) (toPathAbsDir absProjectDir)
        let replacements =
              [ ("__waspModuleName__", name),
                ("__waspModuleCamelName__", camelName)
              ]
        replaceModulePlaceholders replacements absProjectDir
      cliSendMessageC $ Msg.Success $ "Module '" ++ name ++ "' initialized in '" ++ dirName ++ "/'."

replaceModulePlaceholders :: [(String, String)] -> Path' Abs (Dir d) -> IO ()
replaceModulePlaceholders replacements dir = do
  files <- IOUtil.listDirectoryDeep dir
  mapM_ replaceInFile files
  where
    replaceInFile relPath = do
      let absPath = dir </> relPath
      content <- IOUtil.readFileStrict absPath
      let replaced = foldl' replacePlaceholder content replacements
      IOUtil.writeFileFromText absPath replaced

    replacePlaceholder content (placeholder, value) =
      T.replace (T.pack placeholder) (T.pack value) content

toCamelCase :: String -> String
toCamelCase = concatMap capitalize . words . map dashToSpace
  where
    dashToSpace c
      | c == '-' || c == '/' || c == '@' = ' '
      | otherwise = c
    capitalize [] = []
    capitalize (c : cs) = toUpper c : cs
