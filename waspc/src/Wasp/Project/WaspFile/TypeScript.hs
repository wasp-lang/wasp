{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Project.WaspFile.TypeScript
  ( analyzeWaspTsFile,
  )
where

import Control.Arrow (left)
import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
  ( Abs,
    File,
    Path',
    Rel,
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
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), getInstallablePackageScriptInProject)
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
import Wasp.Util (indent)
import Wasp.Util.Aeson (encodeToString)
import qualified Wasp.Util.IO as IOUtil

analyzeWaspTsFile ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspTsFile compileOptions prismaSchemaAst waspFilePath = do
  specResultOrErrors <- runWaspSpecAnalyzer compileOptions prismaSchemaAst waspTsConfigFile waspFilePath
  return $ case specResultOrErrors of
    Left errs -> Left errs
    Right (SpecAnalysisError specErrorMessage) ->
      Left [formatSpecAnalysisError specErrorMessage]
    Right (SpecAnalysisOk specDecls) ->
      (++ specDecls) <$> entityDeclsOrErrors
  where
    waspTsConfigFile = fromJust tsConfigPathsInWaspTsProjects.waspTsConfig

    entityDeclsOrErrors =
      left (map fst) $
        left (map Analyzer.getErrorMessageAndCtx) $
          Analyzer.getEntityDecls prismaSchemaAst

    formatSpecAnalysisError specErrorMessage =
      "Error while analyzing spec (*.wasp.ts) files: " ++ specErrorMessage

runWaspSpecAnalyzer ::
  CompileOptions ->
  Psl.Schema.Schema ->
  Path' (Rel WaspProjectDir) (File WaspTsConfigFile) ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] SpecAnalysisResult)
runWaspSpecAnalyzer compileOptions prismaSchemaAst waspTsConfigFile waspFilePath = do
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
            fromAbsFile absSpecResultFile,
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
    ExitSuccess -> readSpecResultFile
  where
    absSpecResultFile = compileOptions.waspProjectDir </> dotWaspDirInWaspProjectDir </> [relfile|spec-result.json|]
    allowedEntityNames = Psl.Schema.Model.getName . Psl.WithCtx.getNode <$> Psl.Schema.getModels prismaSchemaAst

    nodeEnvForBuildType :: BuildType.BuildType -> String
    nodeEnvForBuildType BuildType.Development = "development"
    nodeEnvForBuildType BuildType.Production = "production"

    readSpecResultFile :: IO (Either [CompileError] SpecAnalysisResult)
    readSpecResultFile = do
      contents <- IOUtil.readFileBytes absSpecResultFile
      return $
        left (\err -> ["Error while reading the spec result from JSON: " ++ err]) $
          Aeson.eitherDecode contents

-- | The result handed back by the spec analyzer subprocess. Mirrors the
-- @SpecResult@ type in @waspc.sh/spec; keep them in sync.
data SpecAnalysisResult
  = SpecAnalysisOk [AS.Decl]
  | SpecAnalysisError String

instance Aeson.FromJSON SpecAnalysisResult where
  parseJSON = Aeson.withObject "SpecAnalysisResult" $ \o -> do
    status <- o Aeson..: "status"
    case status :: String of
      "ok" -> SpecAnalysisOk <$> o Aeson..: "value"
      "error" -> SpecAnalysisError <$> o Aeson..: "error"
      _ -> fail $ "Unknown spec result status: " <> status
