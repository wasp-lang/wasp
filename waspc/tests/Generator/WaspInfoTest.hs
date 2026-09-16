module Generator.WaspInfoTest where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe, (.:))
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec
import qualified Wasp.AppSpec.App.Deployment as Deployment
import Wasp.Generator.WaspInfo (WaspInfo (..))
import qualified Wasp.Project.BuildType as BuildType

spec_WaspInfo :: Spec
spec_WaspInfo = do
  describe "WaspInfo JSON" $ do
    it "writes the deployment mode by name" $ do
      let json = Aeson.toJSON waspInfo
      parseMaybe (Aeson.withObject "WaspInfo" (.: "deploymentMode")) json
        `shouldBe` Just ("single" :: String)

    it "reads back what it wrote" $ do
      Aeson.decode (Aeson.encode waspInfo) `shouldBe` Just waspInfo
  where
    waspInfo =
      WaspInfo
        { waspVersion = "0.26.0",
          generatedAt = UTCTime (fromGregorian 2026 1 1) 0,
          buildType = BuildType.Production,
          deploymentMode = Deployment.Single
        }
