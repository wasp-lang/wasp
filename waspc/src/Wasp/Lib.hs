module Wasp.Lib
  ( compile,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
    findAndAnalyzeWaspFile,
  )
where

import Control.Arrow
import Control.Monad.Except
import Data.List (find, isSuffixOf)
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import StrongPath (Abs, Dir, File', Path', Rel, fromAbsDir, fromAbsFile, relfile, toFilePath, (</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
import Wasp.AppSpec (AppSpec (externalClientFiles), Decl)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Common (DbMigrationsDir, WaspProjectDir, dbMigrationsDirInWaspProjectDir)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter), sendMessage)
import qualified Wasp.CompileOptions as CompileOptions
import Wasp.Error (showCompilerErrorForTerminal)
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator as Generator
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (dotEnvServer)
import Wasp.Generator.WebAppGenerator.Common (dotEnvClient)
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

type CompileWarning = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  appSpecOrAnalyzerErrors <- findAndAnalyzeWaspFile waspDir options
  compileWarnings <- warnIfDotEnvPresent waspDir
  compilerWarningsAndErrors <- case appSpecOrAnalyzerErrors of
    Left analyzerErrors -> return ([], toList analyzerErrors)
    Right appSpec ->
      case ASV.validateAppSpec appSpec of
        [] -> do
          (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
          return (map show $ generatorWarningsFilter options generatorWarnings, map show generatorErrors)
        validationErrors -> return ([], map show validationErrors)
  return $ (compileWarnings, []) <> compilerWarningsAndErrors

findAndAnalyzeWaspFile ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO (Either (NonEmpty CompileError) AppSpec)
findAndAnalyzeWaspFile waspDir options = runExceptT $ do
  waspFilePath <- ExceptT $ left pure <$> findWaspFile waspDir
  declarations <- ExceptT $ analyzeWaspFileContent waspFilePath
  liftIO $ constructAppSpec waspDir options declarations

-- | Checks the wasp directory for potential problems, and issues warnings if any are found.
warnIfDotEnvPresent :: Path' Abs (Dir WaspProjectDir) -> IO [CompileWarning]
warnIfDotEnvPresent waspDir = do
  maybeDotEnvFile <- findDotEnv waspDir
  return $ case maybeDotEnvFile of
    Nothing -> []
    Just _ -> ["Wasp .env files should be named .env.server or .env.client, depending on their use."]

analyzeWaspFileContent :: Path' Abs File' -> IO (Either (NonEmpty CompileError) [Decl])
analyzeWaspFileContent waspFilePath = do
  waspFileContent <- readFile (fromAbsFile waspFilePath)
  return $ case Analyzer.analyze waspFileContent of
    Right decls -> Right decls
    Left analyzeError ->
      Left $
        fromList
          [ showCompilerErrorForTerminal
              (waspFilePath, waspFileContent)
              (getErrorMessageAndCtx analyzeError)
          ]

constructAppSpec :: Path' Abs (Dir WaspProjectDir) -> CompileOptions -> [Decl] -> IO AS.AppSpec
constructAppSpec waspDir options decls = do
  externalServerCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalServerCodeDirPath options)
  externalClientCodeFiles <-
    ExternalCode.readFiles (CompileOptions.externalClientCodeDirPath options)
  maybeDotEnvServerFile <- findDotEnvServer waspDir
  maybeDotEnvClientFile <- findDotEnvClient waspDir
  maybeMigrationsDir <- findMigrationsDir waspDir
  return
    AS.AppSpec
      { AS.decls = decls,
        AS.externalClientFiles = externalClientCodeFiles,
        AS.externalServerFiles = externalServerCodeFiles,
        AS.migrationsDir = maybeMigrationsDir,
        AS.dotEnvServerFile = maybeDotEnvServerFile,
        AS.dotEnvClientFile = maybeDotEnvClientFile,
        AS.isBuild = CompileOptions.isBuild options
      }

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ maybeToEither "Couldn't find a single *.wasp file." $ (waspDir </>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` toFilePath path
        && (length (toFilePath path) > length (".wasp" :: String))

findDotEnvServer :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvServer waspDir = findFileInWaspProjectDir waspDir dotEnvServer

findDotEnvClient :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvClient waspDir = findFileInWaspProjectDir waspDir dotEnvClient

findDotEnv :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnv waspDir = findFileInWaspProjectDir waspDir [relfile|.env|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir </> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing
