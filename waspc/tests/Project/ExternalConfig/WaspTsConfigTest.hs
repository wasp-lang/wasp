module Project.ExternalConfig.WaspTsConfigTest (spec_WaspTsConfig) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.TsConfig (validateTsConfig)
import Wasp.Project.ExternalConfig.WaspTsConfig (waspTsConfigValidator)

spec_WaspTsConfig :: Spec
spec_WaspTsConfig = do
  describe "waspTsConfigValidator" $ do
    it "returns no errors for a valid tsconfig" $
      validate validTsConfig `shouldBe` []

    it "returns an error when a compilerOption has a wrong value" $
      assertReturnsValidationErrorMentioningField "strict" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.strict = Just False})}

    it "returns an error when allowJs has a wrong value" $
      assertReturnsValidationErrorMentioningField "allowJs" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.allowJs = Just False})}

    it "returns an error when a compilerOption is missing" $
      assertReturnsValidationErrorMentioningField "noEmit" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.noEmit = Nothing})}

    it "returns an error when allowJs is missing" $
      assertReturnsValidationErrorMentioningField "allowJs" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.allowJs = Nothing})}

    it "returns an error when compilerOptions is missing" $
      assertReturnsValidationErrorMentioningField "compilerOptions" $
        validTsConfig {T.compilerOptions = Nothing}

    it "returns an error when the @src path mapping is missing" $
      assertReturnsValidationErrorMentioningField "paths" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.paths = Just Map.empty})}

    it "returns an error when the @src path mapping has a wrong value" $
      assertReturnsValidationErrorMentioningField "paths" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.paths = Just $ Map.singleton "@src/*" ["src/*"]})}

    it "returns an error when include is wrong" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["src"]}

validate :: T.TsConfig -> [String]
validate = validateTsConfig waspTsConfigValidator "tsconfig.wasp.json"

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validate config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["main.wasp.ts", "**/*.wasp.ts"],
      T.exclude = Nothing,
      T.files = Nothing,
      T.references = Nothing
    }

validCompilerOptions :: T.CompilerOptions
validCompilerOptions =
  T.CompilerOptions
    { T._module = Just "esnext",
      T.target = Just "ES2022",
      T.composite = Nothing,
      T.skipLibCheck = Just True,
      T.moduleResolution = Just "bundler",
      T.moduleDetection = Just "force",
      T.isolatedModules = Just True,
      T.jsx = Just "preserve",
      T.strict = Just True,
      T.esModuleInterop = Nothing,
      T.lib = Just ["ES2023"],
      T.paths = Just $ Map.singleton "@src/*" ["./src/*"],
      T.allowJs = Just True,
      T.outDir = Nothing,
      T.noEmit = Just True
    }
