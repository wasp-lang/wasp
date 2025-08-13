module Wasp.Project.WaspFile.TypeScript
  ( analyzeWaspTsFile,
  )
where

import Control.Arrow (left)
import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT)
import qualified Data.Aeson as Aeson
import StrongPath
  ( Abs,
    Dir,
    File,
    File',
    Path',
    Rel,
    basename,
    castFile,
    fromAbsDir,
    fromAbsFile,
    relfile,
    (</>),
  )
import System.Exit (ExitCode (..))
import qualified Wasp.Analyzer as Analyzer
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Decl.JSON ()
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
    WaspTsFile,
    dotWaspDirInWaspProjectDir,
  )
import qualified Wasp.Psl.Ast.Model as Psl.Schema.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Util (orElse)
import Wasp.Util.Aeson (encodeToString)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.StrongPath (replaceRelExtension)

data CompiledWaspJsFile

data AppSpecDeclsJsonFile

analyzeWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspTsFile waspProjectDir prismaSchemaAst waspFilePath = runExceptT $ do
  -- TODO: I'm not yet sure where tsconfig.wasp.json location should come from
  -- because we also need that knowledge when generating a TS SDK project.
  compiledWaspJsFile <- ExceptT $ compileWaspTsFile waspProjectDir [relfile|tsconfig.wasp.json|] waspFilePath
  declsJsonFile <- ExceptT $ executeMainWaspJsFileAndGetDeclsFile waspProjectDir prismaSchemaAst compiledWaspJsFile
  ExceptT $ readDecls prismaSchemaAst declsJsonFile

compileWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] (Path' Abs (File CompiledWaspJsFile)))
compileWaspTsFile waspProjectDir tsconfigNodeFileInWaspProjectDir waspFilePath = do
  chan <- newChan
  (_, tscExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      ( runNodeCommandAsJob
          waspProjectDir
          "npx"
          -- We're using tsc to compile the *.wasp.ts file into a JS file.
          --
          -- The tsconfig.wasp.json is configured to give our users with the
          -- best possible IDE support while coding the *.wasp.ts file.
          --
          -- When we actually want to compile the *.wasp.ts file, we must
          -- override some of those rules.
          --
          -- Tehnically, some overrides could have been specified
          -- in the tsconfig.wasp.json file, but we decided to keep them here
          -- because it helps users avoid accidentally breaking things.
          [ "tsc",
            "-p",
            fromAbsFile (waspProjectDir </> tsconfigNodeFileInWaspProjectDir),
            -- The tsconfig.wasp.json file has the noEmit flag on.
            -- The file only exists IDE support, and we don't want users to
            -- accidentally chage the outDir.
            --
            -- Here, to actually generate the JS file in the desired location,
            -- we must turn off the noEmit flag and specify the outDir.
            "--noEmit",
            "false",
            "--outDir",
            fromAbsDir outDir
          ]
          J.Wasp
          chan
      )
  return $ case tscExitCode of
    ExitFailure _status -> Left ["Got TypeScript compiler errors for " ++ fromAbsFile waspFilePath ++ "."]
    ExitSuccess -> Right absCompiledWaspJsFile
  where
    outDir = waspProjectDir </> dotWaspDirInWaspProjectDir
    -- We know this will be the output JS file's location because it's how TSC
    -- works (assuming we've specified the outDir, which we did).
    absCompiledWaspJsFile = outDir </> compiledWaspJsFileInDotWaspDir
    compiledWaspJsFileInDotWaspDir =
      castFile $
        replaceRelExtension (basename waspFilePath) ".js"
          `orElse` error ("Couldn't calculate the compiled JS file path for " ++ fromAbsFile waspFilePath ++ ".")

executeMainWaspJsFileAndGetDeclsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  Path' Abs (File CompiledWaspJsFile) ->
  IO (Either [CompileError] (Path' Abs (File AppSpecDeclsJsonFile)))
executeMainWaspJsFileAndGetDeclsFile waspProjectDir prismaSchemaAst absCompiledMainWaspJsFile = do
  chan <- newChan
  (_, runExitCode) <- do
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      ( runNodeCommandAsJob
          waspProjectDir
          "npx"
          -- TODO: Figure out how to keep running instructions in a single
          -- place (e.g., this is string the same as the package name, but it's
          -- repeated in two places).
          -- Before this, I had the entrypoint file hardcoded, which was bad
          -- too: waspProjectDir </> [relfile|node_modules/wasp-config/dist/run.js|]
          [ "wasp-config",
            fromAbsFile absCompiledMainWaspJsFile,
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
    ExitFailure _status -> return $ Left ["Error while running the compiled *.wasp.ts file."]
    ExitSuccess -> return $ Right absDeclsOutputFile
  where
    absDeclsOutputFile = waspProjectDir </> dotWaspDirInWaspProjectDir </> [relfile|decls.json|]
    allowedEntityNames = Psl.Schema.Model.getName . Psl.WithCtx.getNode <$> Psl.Schema.getModels prismaSchemaAst

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
