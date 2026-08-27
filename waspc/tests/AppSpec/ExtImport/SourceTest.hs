module AppSpec.ExtImport.SourceTest where

import Data.Either (isLeft)
import StrongPath (relfileP)
import Test.Hspec
import Wasp.AppSpec.ExtImport.Source

spec_ExtImportSourceTest :: Spec
spec_ExtImportSourceTest = do
  describe "parseProjectSrcExtImportPath" $ do
    it "parses paths rooted at @src" $ do
      parseProjectSrcExtImportPath "@src/features/page.tsx"
        `shouldBe` Right [relfileP|features/page.tsx|]
    it "rejects paths without the @src prefix" $ do
      parseProjectSrcExtImportPath "features/page.tsx"
        `shouldSatisfy` isLeft
