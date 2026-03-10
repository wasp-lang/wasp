module Wasp.Project.ExternalConfig
  ( readAndValidateExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (Abs, Dir, File, Path', Rel)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.ExternalConfig.TsConfig (TsConfig)
import Wasp.Project.Common
  ( CompileError,
    SrcTsConfigFile,
    WaspProjectDir,
  )
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (readSrcTsConfigFile)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)
import qualified Wasp.Valid.Validator as V

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _srcTsConfig :: TsConfig
  }
  deriving (Show)

readAndValidateExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] ExternalConfigs)
readAndValidateExternalConfigs waspDir srcTsConfigPath = do
  readExternalConfigs waspDir srcTsConfigPath >>= \case
    Left readError -> return $ Left [readError]
    Right externalConfigs ->
      case validateExternalConfigs externalConfigs of
        errors@(_ : _) -> return $ Left errors
        [] -> return $ Right externalConfigs

readExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either CompileError ExternalConfigs)
readExternalConfigs waspDir srcTsConfigPath = runExceptT $ do
  packageJsonContent <- ExceptT $ readPackageJsonFile waspDir
  srcTsConfigContent <- ExceptT $ readSrcTsConfigFile waspDir srcTsConfigPath
  ExceptT $ validateViteConfig waspDir

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _srcTsConfig = srcTsConfigContent
      }

validateExternalConfigs :: ExternalConfigs -> [CompileError]
validateExternalConfigs configs =
  validateSrcTsConfig (_srcTsConfig configs)

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
            V.inField ("compilerOptions", T.compilerOptions) compilerOptionsValidator
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
