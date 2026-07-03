module Project.ExternalConfig.WaspTsConfigTest (spec_WaspTsConfig) where

import Data.List (isInfixOf)
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

    it "returns an error when include is missing a required glob" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["**/*.wasp.ts", "**/*.wasp.tsx"]}

    it "returns an error when include is wrong" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["src"]}

    it "returns no errors when include has the required globs plus extra ones" $
      validate (validTsConfig {T.include = Just ["**/*.wasp.ts", "**/*.wasp.tsx", ".wasp/out/types/spec", "lib/**/*.ts"]})
        `shouldBe` []

    it "returns an error when types is missing the required node entry" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["react"]})}

    it "returns an error when types is missing" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Nothing})}

    it "accepts extra entries in types as long as node is present" $
      validate (validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["node", "vitest/globals"]})})
        `shouldBe` []

validate :: T.TsConfig -> [String]
validate = validateTsConfig waspTsConfigValidator "tsconfig.wasp.json"

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validate config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["**/*.wasp.ts", "**/*.wasp.tsx", ".wasp/out/types/spec"],
      T.exclude = Nothing,
      T.files = Nothing,
      T.references = Nothing
    }

validCompilerOptions :: T.CompilerOptions
validCompilerOptions =
  T.CompilerOptions
    { T._module = Just "esnext",
      T.target = Just "ES2025",
      T.composite = Nothing,
      T.skipLibCheck = Just True,
      T.moduleResolution = Just "bundler",
      T.moduleDetection = Just "force",
      T.isolatedModules = Just True,
      T.jsx = Just "react-jsx",
      T.strict = Just True,
      T.esModuleInterop = Nothing,
      T.lib = Just ["ES2025"],
      T.types = Just ["node"],
      T.paths = Nothing,
      T.allowJs = Just True,
      T.outDir = Nothing,
      T.noEmit = Just True
    }
