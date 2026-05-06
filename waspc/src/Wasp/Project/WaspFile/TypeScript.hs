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
import StrongPath
  ( Abs,
    Dir,
    File,
    File',
    Path',
    Rel,
    basename,
    castFile,
    fromAbsFile,
    fromRelFile,
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
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), ensurePackageIsAtInstallationPathInProject, getInstallablePackageScriptInProject)
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

data RewrittenWaspTsFile

data AppSpecDeclsJsonFile

analyzeWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Psl.Schema.Schema ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] [AS.Decl])
analyzeWaspTsFile waspProjectDir prismaSchemaAst waspFilePath = runExceptT $ do
  -- TODO: I'm not yet sure where tsconfig.wasp.json location should come from
  -- because we also need that knowledge when generating a TS SDK project.
  liftIO $ ensurePackageIsAtInstallationPathInProject waspProjectDir WaspConfigPackage
  (rewrittenWaspTsFile, compilationTsConfigFile) <- ExceptT $ prepareWaspTsFileForCompilation waspProjectDir [relfile|tsconfig.wasp.json|] waspFilePath
  compiledWaspJsFile <- ExceptT $ compileWaspTsFile waspProjectDir compilationTsConfigFile rewrittenWaspTsFile
  declsJsonFile <- ExceptT $ executeMainWaspJsFileAndGetDeclsFile waspProjectDir prismaSchemaAst compiledWaspJsFile
  ExceptT $ readDecls prismaSchemaAst declsJsonFile

prepareWaspTsFileForCompilation ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  Path' Abs (File WaspTsFile) ->
  IO (Either [CompileError] (Path' Abs (File RewrittenWaspTsFile), Path' (Rel WaspProjectDir) File'))
prepareWaspTsFileForCompilation waspProjectDir tsconfigNodeFileInWaspProjectDir waspFilePath = do
  let rewrittenWaspTsFile = waspProjectDir </> rewrittenWaspTsFileInDotWaspDir
      compilationTsConfigFile = waspProjectDir </> compilationTsConfigFileInDotWaspDir
  rewriteResult <- rewriteWaspTsFile waspProjectDir waspFilePath rewrittenWaspTsFile
  case rewriteResult of
    Left errors -> return $ Left errors
    Right () -> do
      IOUtil.writeFile compilationTsConfigFile $ mkCompilationTsConfigJson tsconfigNodeFileInWaspProjectDir rewrittenWaspTsFile
      return $ Right (rewrittenWaspTsFile, compilationTsConfigFileInDotWaspDir)

rewriteWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (File WaspTsFile) ->
  Path' Abs (File RewrittenWaspTsFile) ->
  IO (Either [CompileError] ())
rewriteWaspTsFile waspProjectDir waspFilePath rewrittenWaspTsFile = do
  chan <- newChan
  (_, rewriteExitCode) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      ( runNodeCommandAsJob
          waspProjectDir
          "node"
          [ fromRelFile $ getInstallablePackageScriptInProject WaspConfigPackage,
            "rewrite",
            fromAbsFile waspFilePath,
            fromAbsFile rewrittenWaspTsFile
          ]
          J.Wasp
          chan
      )
  return $ case rewriteExitCode of
    ExitFailure _status -> Left ["Error while rewriting " ++ fromAbsFile waspFilePath ++ "."]
    ExitSuccess -> Right ()

mkCompilationTsConfigJson ::
  Path' (Rel WaspProjectDir) File' ->
  Path' Abs (File RewrittenWaspTsFile) ->
  String
mkCompilationTsConfigJson tsconfigNodeFileInWaspProjectDir rewrittenWaspTsFile =
  unlines
    [ "{",
      "  \"extends\": \"../" ++ fromRelFile tsconfigNodeFileInWaspProjectDir ++ "\",",
      "  \"compilerOptions\": {",
      "    \"noEmit\": false",
      "  },",
      "  \"include\": [\"" ++ fromRelFile (basename rewrittenWaspTsFile) ++ "\"]",
      "}"
    ]

compileWaspTsFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  Path' Abs (File sourceWaspTsFile) ->
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
            -- The generated tsconfig turns off the noEmit flag from
            -- tsconfig.wasp.json, so tsc emits the JS file next to the
            -- rewritten TS file in .wasp/.
            "--noEmit",
            "false"
          ]
          J.Wasp
          chan
      )
  return $ case tscExitCode of
    ExitFailure _status -> Left ["Got TypeScript compiler errors for " ++ fromAbsFile waspFilePath ++ "."]
    ExitSuccess -> Right absCompiledWaspJsFile
  where
    -- We know this will be the output JS file's location because it's how TSC
    -- works: it emits the JS file next to the rewritten TS file in .wasp/.
    absCompiledWaspJsFile = waspProjectDir </> dotWaspDirInWaspProjectDir </> compiledWaspJsFileInDotWaspDir
    compiledWaspJsFileInDotWaspDir =
      castFile $
        replaceRelExtension (basename waspFilePath) ".js"
          `orElse` error ("Couldn't calculate the compiled JS file path for " ++ fromAbsFile waspFilePath ++ ".")

rewrittenWaspTsFileInDotWaspDir :: Path' (Rel WaspProjectDir) (File RewrittenWaspTsFile)
rewrittenWaspTsFileInDotWaspDir = dotWaspDirInWaspProjectDir </> [relfile|main.wasp.rewritten.ts|]

compilationTsConfigFileInDotWaspDir :: Path' (Rel WaspProjectDir) File'
compilationTsConfigFileInDotWaspDir = dotWaspDirInWaspProjectDir </> [relfile|tsconfig.wasp.generated.json|]

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
      -- We invoke the script directly via `node` instead of `npx` because
      -- `npx` requires the bin file to be executable, and `cabal install`
      -- strips executable permissions from data files.
      ( runNodeCommandAsJob
          waspProjectDir
          "node"
          [ fromRelFile $ getInstallablePackageScriptInProject WaspConfigPackage,
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
