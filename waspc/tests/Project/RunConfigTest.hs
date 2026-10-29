module Project.RunConfigTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.Project.RunConfig (makeLocalUrl)

spec_makeLocalUrl :: Spec
spec_makeLocalUrl =
  describe "makeLocalUrl" $
    it "builds a localhost URL from the port and the optional path" $ do
      show (makeLocalUrl 3001 Nothing) `shouldBe` "http://localhost:3001"
      show (makeLocalUrl 3000 (Just [absdirP|/app/|])) `shouldBe` "http://localhost:3000/app/"
