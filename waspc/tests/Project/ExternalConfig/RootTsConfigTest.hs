module Project.ExternalConfig.RootTsConfigTest (spec_RootTsConfig) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.RootTsConfig (validateRootTsConfig)

spec_RootTsConfig :: Spec
spec_RootTsConfig = do
  describe "validateRootTsConfig" $ do
    it "returns no errors for a valid tsconfig" $ do
      validateRootTsConfig "tsconfig.json" validTsConfig `shouldBe` []

    it "doesn't care about the './' prefix for references" $ do
      let config =
            validTsConfig
              { T.references =
                  Just
                    [ T.TsConfigReference {T.path = "tsconfig.src.json"},
                      T.TsConfigReference {T.path = "./tsconfig.wasp.json"}
                    ]
              }
      validateRootTsConfig "tsconfig.json" config `shouldBe` []

    it "returns an error when files is missing" $
      assertReturnsValidationErrorMentioningField "files" $
        validTsConfig {T.files = Nothing}

    it "returns an error when files is non-empty" $
      assertReturnsValidationErrorMentioningField "files" $
        validTsConfig {T.files = Just ["main.ts"]}

    it "returns an error when references is missing" $
      assertReturnsValidationErrorMentioningField "references" $
        validTsConfig {T.references = Nothing}

    it "returns an error when a required reference is missing" $
      assertReturnsValidationErrorMentioningField "references" $
        validTsConfig
          { T.references = Just [T.TsConfigReference {T.path = "tsconfig.src.json"}]
          }

assertReturnsValidationErrorMentioningField :: String -> T.TsConfig -> Expectation
assertReturnsValidationErrorMentioningField fieldName config =
  validateRootTsConfig "tsconfig.json" config `shouldSatisfy` any (fieldName `isInfixOf`)

validTsConfig :: T.TsConfig
validTsConfig =
  T.TsConfig
    { T.compilerOptions = Nothing,
      T.include = Nothing,
      T.files = Just [],
      T.references =
        Just
          [ T.TsConfigReference {T.path = "tsconfig.src.json"},
            T.TsConfigReference {T.path = "tsconfig.wasp.json"}
          ]
    }
