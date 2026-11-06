module Node.VersionTest where

import Test.Hspec
import Wasp.Generator.DepVersions.Catalog (catalogVersionRange)
import Wasp.Node.Version (nodeTypesVersionRangeMatchingNodeMajor, oldestWaspSupportedNodeVersion)

spec_NodeVersionTest :: Spec
spec_NodeVersionTest = do
  describe "nodeTypesVersionRangeMatchingNodeMajor" $ do
    it "matches the @types/node version in the generated-app pnpm catalog" $ do
      show (nodeTypesVersionRangeMatchingNodeMajor oldestWaspSupportedNodeVersion)
        `shouldBe` show $$(catalogVersionRange "@types/node")
