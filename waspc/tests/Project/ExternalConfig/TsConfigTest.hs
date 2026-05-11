module Project.ExternalConfig.TsConfigTest (spec_TsConfig) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.ExternalConfig.TsConfig (validateTsConfig)
import qualified Wasp.Validator as V

spec_TsConfig :: Spec
spec_TsConfig = do
  describe "validateTsConfig" $ do
    it "includes the given file name in error messages" $ do
      let alwaysFails = const $ V.failure "boom"
      validateTsConfig alwaysFails "custom-name.json" anyTsConfig
        `shouldSatisfy` any ("custom-name.json" `isInfixOf`)

anyTsConfig :: T.TsConfig
anyTsConfig =
  T.TsConfig
    { T.compilerOptions = Nothing,
      T.include = Nothing,
      T.files = Nothing,
      T.references = Nothing
    }
