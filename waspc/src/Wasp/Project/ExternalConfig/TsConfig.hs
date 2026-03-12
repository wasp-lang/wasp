module Wasp.Project.ExternalConfig.TsConfig
  ( parseAndValidateSrcTsConfig,
    parseAndValidateRootTsConfig,
    parseAndValidateWaspTsConfig,

    -- * Exported for testing only
    validateSrcTsConfig,
    validateRootTsConfig,
    validateWaspTsConfig,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile, toFilePath)
import Wasp.ExternalConfig.TsConfig (TsConfigFile, parseTsConfigFile)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, RootTsConfigFile, SrcTsConfigFile, WaspProjectDir, WaspTsConfigFile, findFileInWaspProjectDir)
import qualified Wasp.Validator as V

parseAndValidateSrcTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] T.TsConfig)
parseAndValidateSrcTsConfig = parseAndValidateTsConfigFile validateSrcTsConfig

parseAndValidateRootTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File RootTsConfigFile) ->
  IO (Either [CompileError] T.TsConfig)
parseAndValidateRootTsConfig = parseAndValidateTsConfigFile validateRootTsConfig

parseAndValidateWaspTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File WaspTsConfigFile) ->
  IO (Either [CompileError] T.TsConfig)
parseAndValidateWaspTsConfig = parseAndValidateTsConfigFile validateWaspTsConfig

parseAndValidateTsConfigFile ::
  (TsConfigFile f) =>
  (T.TsConfig -> [CompileError]) ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File f) ->
  IO (Either [CompileError] T.TsConfig)
parseAndValidateTsConfigFile validate waspDir tsConfigPath =
  findAndParseTsConfigFile waspDir tsConfigPath >>= \case
    Left err -> return $ Left [err]
    Right tsConfig ->
      case validate tsConfig of
        [] -> return $ Right tsConfig
        errors -> return $ Left errors

findAndParseTsConfigFile ::
  (TsConfigFile f) =>
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File f) ->
  IO (Either String T.TsConfig)
findAndParseTsConfigFile waspDir srcTsConfigPath = runExceptT $ do
  tsConfigFile <- ExceptT findTsConfigOrError
  ExceptT $ parseTsConfigFile tsConfigFile
  where
    findTsConfigOrError = maybeToEither fileNotFoundMessage <$> findFileInWaspProjectDir waspDir srcTsConfigPath
    fileNotFoundMessage = "Couldn't find " ++ fromRelFile srcTsConfigPath ++ " in the " ++ toFilePath waspDir ++ " directory"

validateSrcTsConfig :: T.TsConfig -> [CompileError]
validateSrcTsConfig config =
  show <$> V.execValidator tsConfigValidator config
  where
    -- References for understanding the required compiler options:
    --   - The comments in templates/sdk/wasp/tsconfig.json
    --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
    --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
    --   - https://www.typescriptlang.org/tsconfig/

    tsConfigValidator :: V.Validator T.TsConfig
    tsConfigValidator =
      V.withFileName "tsconfig.json" $
        V.all
          [ V.inField ("include", T.include) $ V.eqJust ["src"],
            V.inField ("compilerOptions", T.compilerOptions) $ V.required compilerOptionsValidator
          ]

    compilerOptionsValidator :: V.Validator T.CompilerOptions
    compilerOptionsValidator =
      V.all
        [ V.inField ("module", T._module) $ V.eqJust "esnext",
          V.inField ("target", T.target) $ V.eqJust "esnext",
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          V.inField ("moduleResolution", T.moduleResolution) $ V.eqJust "bundler",
          V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
          V.inField ("jsx", T.jsx) $ V.eqJust "preserve",
          V.inField ("strict", T.strict) $ V.eqJust True,
          V.inField ("esModuleInterop", T.esModuleInterop) $ V.eqJust True,
          V.inField ("lib", T.lib) $ V.eqJust ["dom", "dom.iterable", "esnext"],
          V.inField ("allowJs", T.allowJs) $ V.eqJust True,
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          V.inField ("outDir", T.outDir) $ V.eqJust ".wasp/out/user",
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          V.inField ("composite", T.composite) $ V.eqJust True,
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True
        ]

-- TODO: remove hardcoded and duplicated paths paths
validateRootTsConfig :: T.TsConfig -> [CompileError]
validateRootTsConfig config =
  show <$> V.execValidator rootTsConfigValidator config
  where
    rootTsConfigValidator :: V.Validator T.TsConfig
    rootTsConfigValidator =
      V.withFileName "tsconfig.json" $
        V.all
          [ V.inField ("files", T.files) $ V.eqJust [],
            V.inField ("references", T.references) $
              V.eqJust
                [ T.TsConfigReference {T.path = "./tsconfig.src.json"},
                  T.TsConfigReference {T.path = "./tsconfig.wasp.json"}
                ]
          ]

validateWaspTsConfig :: T.TsConfig -> [CompileError]
validateWaspTsConfig config =
  show <$> V.execValidator waspTsConfigValidator config
  where
    waspTsConfigValidator :: V.Validator T.TsConfig
    waspTsConfigValidator =
      V.withFileName "tsconfig.wasp.json" $
        V.all
          [ V.inField ("include", T.include) $ V.eqJust ["main.wasp.ts"],
            V.inField ("compilerOptions", T.compilerOptions) $ V.required waspCompilerOptionsValidator
          ]

    waspCompilerOptionsValidator :: V.Validator T.CompilerOptions
    waspCompilerOptionsValidator =
      V.all
        [ V.inField ("target", T.target) $ V.eqJust "ES2022",
          V.inField ("module", T._module) $ V.eqJust "NodeNext",
          V.inField ("strict", T.strict) $ V.eqJust True,
          V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
          V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True,
          V.inField ("noEmit", T.noEmit) $ V.eqJust True,
          V.inField ("lib", T.lib) $ V.eqJust ["ES2023"]
        ]
