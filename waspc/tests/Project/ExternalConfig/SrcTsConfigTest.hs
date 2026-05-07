module Project.ExternalConfig.SrcTsConfigTest (spec_SrcTsConfig) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.SrcTsConfig (srcTsConfigValidator)
import Wasp.Project.ExternalConfig.TsConfig (validateTsConfig)

spec_SrcTsConfig :: Spec
spec_SrcTsConfig = do
  describe "srcTsConfigValidator" $ do
    it "returns no errors for a valid tsconfig" $
      validate validTsConfig `shouldBe` []

    it "returns an error when a compilerOption has a wrong value" $
      assertReturnsValidationErrorMentioningField "strict" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.strict = Just False})}

    it "returns an error when a compilerOption is missing" $
      assertReturnsValidationErrorMentioningField "jsx" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.jsx = Nothing})}

    it "returns an error when include is wrong" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["lib"]}

validate :: T.TsConfig -> [String]
validate = validateTsConfig srcTsConfigValidator "tsconfig.json"

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validate config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["src"],
      T.files = Nothing,
      T.references = Nothing
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
      T.allowJs = Just True,
      T.outDir = Just ".wasp/out/user",
      T.noEmit = Nothing
    }
