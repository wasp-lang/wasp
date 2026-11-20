module Project.RunConfigTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.Project.RunConfig (makeLocalUrl)
import Wasp.Util.Network.URI (showUri)

spec_makeLocalUrl :: Spec
spec_makeLocalUrl =
  describe "makeLocalUrl" $
    it "builds a localhost URL from the port and the optional path" $ do
      showUri (makeLocalUrl 3001 Nothing) `shouldBe` "http://localhost:3001"
      showUri (makeLocalUrl 3000 (Just [absdirP|/app/|])) `shouldBe` "http://localhost:3000/app/"
