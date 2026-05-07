module Project.ExternalConfig.WaspTsConfigTest (spec_WaspTsConfig) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.WaspTsConfig (validateWaspTsConfig)

spec_WaspTsConfig :: Spec
spec_WaspTsConfig = do
  describe "validateWaspTsConfig" $ do
    it "returns no errors for a valid tsconfig" $ do
      validateWaspTsConfig "tsconfig.wasp.json" validTsConfig `shouldBe` []

    it "returns an error when a compilerOption has a wrong value" $
      assertReturnsValidationErrorMentioningField "strict" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.strict = Just False})}

    it "returns an error when a compilerOption is missing" $
      assertReturnsValidationErrorMentioningField "noEmit" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.noEmit = Nothing})}

    it "returns an error when compilerOptions is missing" $
      assertReturnsValidationErrorMentioningField "compilerOptions" $
        validTsConfig {T.compilerOptions = Nothing}

    it "returns an error when include is wrong" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["src"]}

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validateWaspTsConfig "tsconfig.wasp.json" config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["main.wasp.ts"],
      T.files = Nothing,
      T.references = Nothing
    }

validCompilerOptions :: T.CompilerOptions
validCompilerOptions =
  T.CompilerOptions
    { T._module = Just "NodeNext",
      T.target = Just "ES2022",
      T.composite = Nothing,
      T.skipLibCheck = Just True,
      T.moduleResolution = Nothing,
      T.moduleDetection = Just "force",
      T.isolatedModules = Just True,
      T.jsx = Nothing,
      T.strict = Just True,
      T.esModuleInterop = Nothing,
      T.lib = Just ["ES2023"],
      T.allowJs = Nothing,
      T.outDir = Nothing,
      T.noEmit = Just True
    }
