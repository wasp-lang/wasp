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
      assertReturnsValidationErrorMentioningField "moduleResolution" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.moduleResolution = Just "nodenext"})}

    it "returns an error when a compilerOption is missing" $
      assertReturnsValidationErrorMentioningField "jsx" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.jsx = Nothing})}

    it "returns an error when `include` is missing a required entry" $
      assertReturnsValidationErrorMentioningField "include" $
        validTsConfig {T.include = Just ["lib"]}

    it "accepts extra entries in `include`" $
      validate (validTsConfig {T.include = Just ["src", ".wasp/out/types/app", "lib"]})
        `shouldBe` []

    it "returns an error when `exclude` is missing" $
      assertReturnsValidationErrorMentioningField "exclude" $
        validTsConfig {T.exclude = Nothing}

    it "accepts extra entries in `exclude`" $
      validate (validTsConfig {T.exclude = Just ["**/*.wasp.ts", "scripts"]})
        `shouldBe` []

    it "returns an error when `types` is missing a required entry" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["node"]})}

    it "returns an error when `types` is missing" $
      assertReturnsValidationErrorMentioningField "types" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Nothing})}

    it "accepts extra entries in `types` as long as `react` and `node` are present" $
      validate (validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.types = Just ["react", "node", "vite/client"]})})
        `shouldBe` []

    it "returns an error when `module` is not bundler-friendly" $
      assertReturnsValidationErrorMentioningField "module" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T._module = Just "commonjs"})}

    it "accepts every `jsx` value matching the bundler's transform" $
      let validateWithJsx jsx =
            validate (validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.jsx = Just jsx})})
       in map validateWithJsx ["preserve", "react-jsx"] `shouldBe` [[], []]

    it "returns an error when `jsx` does not match the bundler's transform" $
      assertReturnsValidationErrorMentioningField "jsx" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.jsx = Just "react"})}

    it "returns an error when `isolatedModules` is off" $
      assertReturnsValidationErrorMentioningField "isolatedModules" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.isolatedModules = Just False})}

    it "returns an error when `moduleDetection` is not `force`" $
      assertReturnsValidationErrorMentioningField "moduleDetection" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.moduleDetection = Nothing})}

    it "returns an error when `noEmit` is `true`" $
      assertReturnsValidationErrorMentioningField "noEmit" $
        validTsConfig {T.compilerOptions = Just (validCompilerOptions {T.noEmit = Just True})}

    it "accepts any values for the options Wasp doesn't require" $
      validate
        ( validTsConfig
            { T.compilerOptions =
                Just
                  ( validCompilerOptions
                      { -- These are deliberately set to values different than the ones we have in starter templates.
                        T.strict = Just False,
                        T.target = Just "es2020",
                        T.lib = Just ["esnext"],
                        T.allowJs = Nothing
                      }
                  )
            }
        )
        `shouldBe` []

validate :: T.TsConfig -> [String]
validate = validateTsConfig srcTsConfigValidator "tsconfig.json"

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validate config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Just validCompilerOptions,
      T.include = Just ["src", ".wasp/out/types/app"],
      T.exclude = Just ["**/*.wasp.ts"],
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
      T.types = Just ["react", "node"],
      T.paths = Nothing,
      T.allowJs = Just True,
      T.outDir = Just ".wasp/out/user",
      T.noEmit = Nothing
    }
