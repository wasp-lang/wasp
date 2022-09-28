module Wasp.Lib
  ( compile,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
    analyzeWaspProject,
  )
where

import Data.List (find, isSuffixOf)
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified Wasp.Analyzer as Analyzer
import Wasp.Analyzer.AnalyzeError (getErrorMessageAndCtx)
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
import qualified Wasp.Util.IO as Util.IO

type CompileError = String

type CompileWarning = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO ([CompileWarning], [CompileError])
compile waspDir outDir options = do
  (compileWarnings, appSpecOrCompileErrors) <- analyzeWaspProject waspDir options
  print compileWarnings
  case appSpecOrCompileErrors of
    Left compileErrors -> return (compileWarnings, toList compileErrors)
    Right appSpec ->
      case ASV.validateAppSpec appSpec of
        [] -> do
          (generatorWarnings, generatorErrors) <- Generator.writeWebAppCode appSpec outDir (sendMessage options)
          let filteredGeneratorWarnings = map show $ generatorWarningsFilter options generatorWarnings
          return (filteredGeneratorWarnings ++ compileWarnings, map show generatorErrors)
        validationErrors -> do
          return (compileWarnings, map show validationErrors)

analyzeWaspProject ::
  Path' Abs (Dir WaspProjectDir) ->
  CompileOptions ->
  IO ([CompileWarning], Either (NonEmpty CompileError) AS.AppSpec)
analyzeWaspProject waspDir options = do
  warnings <- warnIfDotEnvPresent waspDir
  maybeWaspFilePath <- findWaspFile waspDir
  case maybeWaspFilePath of
    Nothing -> return (warnings, Left $ fromList ["Couldn't find a single *.wasp file."])
    Just waspFilePath -> do
      waspFileContent <- readFile (SP.fromAbsFile waspFilePath)
      case Analyzer.analyze waspFileContent of
        Left analyzeError ->
          return
            ( warnings,
              Left $
                fromList
                  [ showCompilerErrorForTerminal
                      (waspFilePath, waspFileContent)
                      (getErrorMessageAndCtx analyzeError)
                  ]
            )
        Right decls -> do
          externalCodeFiles <-
            ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
          maybeDotEnvServerFile <- findDotEnvServer waspDir
          maybeDotEnvClientFile <- findDotEnvClient waspDir
          maybeMigrationsDir <- findMigrationsDir waspDir
          return
            ( warnings,
              Right
                AS.AppSpec
                  { AS.decls = decls,
                    AS.externalCodeFiles = externalCodeFiles,
                    AS.externalCodeDirPath = CompileOptions.externalCodeDirPath options,
                    AS.migrationsDir = maybeMigrationsDir,
                    AS.dotEnvServerFile = maybeDotEnvServerFile,
                    AS.dotEnvClientFile = maybeDotEnvClientFile,
                    AS.isBuild = CompileOptions.isBuild options
                  }
            )

-- | Checks the wasp directory for potential problems, and issues warnings if any are found.
warnIfDotEnvPresent :: Path' Abs (Dir WaspProjectDir) -> IO [CompileWarning]
warnIfDotEnvPresent waspDir = do
  maybeDotEnvFile <- findDotEnv waspDir
  case maybeDotEnvFile of
    Nothing -> return []
    Just _ -> return ["Wasp .env files should be named .env.server or .env.client, depending on their use."]

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` SP.toFilePath path
        && (length (SP.toFilePath path) > length (".wasp" :: String))

findDotEnvServer :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvServer waspDir = findFileInWaspProjectDir waspDir dotEnvServer

findDotEnvClient :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvClient waspDir = findFileInWaspProjectDir waspDir dotEnvClient

findDotEnv :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnv waspDir = findFileInWaspProjectDir waspDir [relfile|.env|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (SP.Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir SP.</> file
  fileExists <- doesFileExist $ SP.toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

findMigrationsDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' Abs (Dir DbMigrationsDir)))
findMigrationsDir waspDir = do
  let migrationsAbsPath = waspDir SP.</> dbMigrationsDirInWaspProjectDir
  migrationsExists <- doesDirectoryExist $ SP.fromAbsDir migrationsAbsPath
  return $ if migrationsExists then Just migrationsAbsPath else Nothing
