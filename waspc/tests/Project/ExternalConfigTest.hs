module Project.ExternalConfigTest (spec_ExternalConfig) where

import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.TsConfig (validateSrcTsConfig)

spec_ExternalConfig :: Spec
spec_ExternalConfig = do
  describe "validateSrcTsConfig" $ do
    it "returns no errors for a valid tsconfig" $ do
      validateSrcTsConfig validTsConfig `shouldBe` []

    it "returns an error when a compilerOption has a wrong value" $ do
      let config = validTsConfig {T.compilerOptions = (T.compilerOptions validTsConfig) {T.strict = Just False}}
      validateSrcTsConfig config `shouldSatisfy` (not . null)

    it "returns an error when a compilerOption is missing" $ do
      let config = validTsConfig {T.compilerOptions = (T.compilerOptions validTsConfig) {T.jsx = Nothing}}
      validateSrcTsConfig config `shouldSatisfy` (not . null)

    it "returns an error when include is wrong" $ do
      let config = validTsConfig {T.include = Just ["lib"]}
      validateSrcTsConfig config `shouldSatisfy` (not . null)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions =
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
            T.outDir = Just ".wasp/out/user"
          },
      T.include = Just ["src"]
    }
