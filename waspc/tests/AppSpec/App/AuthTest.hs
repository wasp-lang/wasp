module AppSpec.App.AuthTest where

import qualified Data.Aeson as Aeson
import Test.Hspec
import qualified Wasp.AppSpec.App.Auth as Auth

spec_enabledAuthMethodNames :: Spec
spec_enabledAuthMethodNames =
  describe "enabledAuthMethodNames" $ do
    it "includes LinkedIn using its public config name" $
      (Auth.enabledAuthMethodNames <$> Aeson.eitherDecode "{\"linkedIn\": {}}")
        `shouldBe` Right ["linkedIn"]

    it "omits LinkedIn when disabled" $
      (Auth.enabledAuthMethodNames <$> Aeson.eitherDecode "{\"linkedIn\": null, \"google\": {}}")
        `shouldBe` Right ["google"]

    it "includes LinkedIn alongside other enabled providers" $
      (Auth.enabledAuthMethodNames <$> Aeson.eitherDecode "{\"gitHub\": {}, \"linkedIn\": {}, \"microsoft\": {}}")
        `shouldBe` Right ["gitHub", "linkedIn", "microsoft"]
