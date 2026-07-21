module Project.ExternalConfig.SrcTsConfigTest (spec_SrcTsConfig) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.SrcTsConfig (moduleSrcTsConfigValidator, srcTsConfigValidator)
import Wasp.Project.ExternalConfig.TsConfig (validateTsConfig)

spec_SrcTsConfig :: Spec
spec_SrcTsConfig = do
  describe "srcTsConfigValidator" $ do
    it "returns no errors for a valid tsconfig" $
      validateApp validTsConfig `shouldBe` []

    it "returns an error when a compilerOption has a wrong value" $
      assertReturnsValidationErrorMentioningField "strict" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.strict = Just False})}

    it "returns an error when a compilerOption is missing" $
      assertReturnsValidationErrorMentioningField "jsx" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.jsx = Nothing})}

    it "returns an error when include is wrong" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["lib"]}

    it "returns an error when exclude is wrong" $
      assertReturnsValidationErrorMentioningField "exclude" $
        validTsConfig {T.exclude = Nothing}

    it "returns an error when types is missing a required entry" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["node"]})}

    it "returns an error when types is missing" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Nothing})}

    it "accepts extra entries in types as long as react and node are present" $
      validateApp (validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["react", "node", "vite/client"]})})
        `shouldBe` []

  describe "moduleSrcTsConfigValidator" $ do
    it "returns no errors for a valid module tsconfig" $
      validateModule validModuleTsConfig `shouldBe` []

    it "reuses the app source compiler option requirements" $
      assertReturnsModuleValidationErrorMentioningField "target" $
        validModuleTsConfig {T.compilerOptions = Just (validModuleCompilerOptions {T.target = Just "ES2022"})}

    it "requires the Wasp SDK shim declaration" $
      assertReturnsModuleValidationErrorMentioningField "include" $
        validModuleTsConfig {T.include = Just ["src"]}

    it "requires noEmit" $
      assertReturnsModuleValidationErrorMentioningField "noEmit" $
        validModuleTsConfig {T.compilerOptions = Just (validModuleCompilerOptions {T.noEmit = Nothing})}

    it "requires JSX to be compiled away in module source" $
      assertReturnsModuleValidationErrorMentioningField "jsx" $
        validModuleTsConfig {T.compilerOptions = Just (validModuleCompilerOptions {T.jsx = Just "preserve"})}

validateApp :: T.TsConfig -> [String]
validateApp = validateTsConfig srcTsConfigValidator "tsconfig.src.json"

validateModule :: T.TsConfig -> [String]
validateModule = validateTsConfig moduleSrcTsConfigValidator "tsconfig.src.json"

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validateApp config `shouldSatisfy` any (fieldName `isInfixOf`)

assertReturnsModuleValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsModuleValidationErrorMentioningField fieldName config =
  validateModule config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["src"],
      T.exclude = Just ["**/*.wasp.ts"],
      T.files = Nothing,
      T.references = Nothing
    }

validModuleTsConfig :: T.TsConfig
validModuleTsConfig =
  validTsConfig
    { T.compilerOptions = Just validModuleCompilerOptions,
      T.include = Just ["src", ".wasp/wasp/ambient.d.ts"]
    }

validCompilerOptions :: T.CompilerOptions
validCompilerOptions =
  T.CompilerOptions
    { T._module = Just "esnext",
      T.target = Just "esnext",
      T.composite = Just True,
      T.skipLibCheck = Just True,
      T.moduleResolution = Just "bundler",
      T.moduleDetection = Just "force",
      T.isolatedModules = Just True,
      T.jsx = Just "preserve",
      T.strict = Just True,
      T.esModuleInterop = Just True,
      T.lib = Just ["dom", "dom.iterable", "esnext"],
      T.types = Just ["react", "node"],
      T.paths = Nothing,
      T.allowJs = Just True,
      T.outDir = Just ".wasp/out/user",
      T.noEmit = Nothing
    }

validModuleCompilerOptions :: T.CompilerOptions
validModuleCompilerOptions =
  validCompilerOptions
    { T.composite = Nothing,
      T.outDir = Nothing,
      T.noEmit = Just True,
      T.jsx = Just "react-jsx"
    }
