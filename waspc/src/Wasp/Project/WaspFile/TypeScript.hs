{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Project.WaspFile.TypeScript
  ( analyzeWaspTsFile,
  )
where

import Control.Arrow (left)
import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
  ( Abs,
    File,
    Path',
    Rel,
    fromAbsDir,
    fromAbsFile,
    fromRelFile,
    relfile,
    (</>),
  )
import System.Exit (ExitCode (..))
import qualified Wasp.Analyzer as Analyzer
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject, getInstallablePackageScriptInProject)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common
  ( CompileError,
    TsConfigPaths (..),
    WaspProjectDir,
    WaspTsConfigFile,
    WaspTsFile,
    dotWaspDirInWaspProjectDir,
    tsConfigPathsInWaspTsProjects,
  )
import qualified Wasp.Psl.Ast.Model as Psl.Schema.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Util.Aeson (encodeToString)
import qualified Wasp.Util.IO as IOUtil

data AppSpecDeclsJsonFile

analyzeWaspTsFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspTsFile compileOptions prismaSchemaAst waspFilePath = runExceptT $ do
  -- TODO: replace this with require WaspSpecAvailable
  liftIO $ ensurePackageIsAtInstallationPathInProject compileOptions.waspProjectDir WaspSpecPackage
  declsJsonFile <- ExceptT $ runWaspSpecAnalyzerAndGetDeclsFile compileOptions prismaSchemaAst waspTsConfigFile waspFilePath
  ExceptT $ readDecls prismaSchemaAst declsJsonFile
  where
    waspTsConfigFile = fromJust tsConfigPathsInWaspTsProjects.waspTsConfig

runWaspSpecAnalyzerAndGetDeclsFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' (Rel WaspProjectDir) (File WaspTsConfigFile) ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] (Path' Abs (File AppSpecDeclsJsonFile)))
runWaspSpecAnalyzerAndGetDeclsFile compileOptions prismaSchemaAst waspTsConfigFile waspFilePath = do
  chan <- newChan
  (_, runExitCode) <- do
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      -- We invoke the script directly via `node` instead of `npx` because
      -- `npx` requires the bin file to be executable, and `cabal install`
      -- strips executable permissions from data files.
      ( runNodeCommandAsJobWithExtraEnv
          [ -- `NODE_ENV` is a convention which allows code to assume what environment it's running in.
            -- Not related to `node` itself, so we have to set it manually.
            -- It enables users to write environment specific code in the TS config.
            -- NOTE: Some consider it an antipattern, but other frameworks/tools (Next.js, Nuxt, Vite)
            --       also provide the `NODE_ENV` values for the "configuration runtime".
            --       Maybe consider using a different key, e.g. `WASP_MODE`?
            ("NODE_ENV", nodeEnvForBuildType compileOptions.buildType)
          ]
          compileOptions.waspProjectDir
          "node"
          [ fromRelFile $ getInstallablePackageScriptInProject WaspSpecPackage,
            "analyze",
            fromAbsFile waspFilePath,
            fromAbsFile (compileOptions.waspProjectDir </> waspTsConfigFile),
            fromAbsDir compileOptions.waspProjectDir,
            fromAbsFile absDeclsOutputFile,
            -- When the user is coding main.wasp.ts, TypeScript must know about
            -- all the available entities to warn the user if they use an
            -- entity that doesn't exist.
            encodeToString allowedEntityNames
          ]
          J.Wasp
          chan
      )
  case runExitCode of
    ExitFailure _status -> return $ Left ["Error while analyzing the *.wasp.ts file."]
    ExitSuccess -> return $ Right absDeclsOutputFile
  where
    absDeclsOutputFile = compileOptions.waspProjectDir </> dotWaspDirInWaspProjectDir </> [relfile|decls.json|]
    allowedEntityNames = Psl.Schema.Model.getName . Psl.WithCtx.getNode <$> Psl.Schema.getModels prismaSchemaAst

    nodeEnvForBuildType :: BuildType.BuildType -> String
    nodeEnvForBuildType BuildType.Development = "development"
    nodeEnvForBuildType BuildType.Production = "production"

readDecls :: Psl.Schema.Schema -> Path' Abs (File AppSpecDeclsJsonFile) -> IO (Either [CompileError] [AS.Decl])
readDecls prismaSchemaAst declsJsonFile = runExceptT $ do
  entityDecls <- liftEither entityDeclsOrErrors
  remainingDecls <- ExceptT $ left (: []) <$> declsFromJsonOrError
  return $ entityDecls ++ remainingDecls
  where
    entityDeclsOrErrors =
      left (map fst) $
        left (map Analyzer.getErrorMessageAndCtx) $
          Analyzer.getEntityDecls prismaSchemaAst

    declsFromJsonOrError = do
      declsBytestring <- IOUtil.readFileBytes declsJsonFile
      return $
        left ("Error while reading the declarations from JSON: " ++) $
          Aeson.eitherDecode declsBytestring
