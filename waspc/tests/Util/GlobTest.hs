module Util.GlobTest where

import qualified StrongPath as SP
import System.FilePath.Glob (match)
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.Glob (descendantsGlob, dirGlob)

spec_globConstruction :: Spec
spec_globConstruction =
  describe "glob construction" $ do
    it "matches only the directory itself with dirGlob" $ do
      let serverSrcGlob = dirGlob [SP.reldir|server/src|]
      match serverSrcGlob (SP.fromRelDir [SP.reldir|server/src|]) `shouldBe` True
      match serverSrcGlob "server/src/a.ts" `shouldBe` False

    it "matches anything at any depth inside the directory with descendantsGlob" $ do
      let serverSrcDescendantsGlob = descendantsGlob [SP.reldir|server/src|]
      match serverSrcDescendantsGlob "server/src/a.ts" `shouldBe` True
      match serverSrcDescendantsGlob "server/src/a/b.ts" `shouldBe` True
      match serverSrcDescendantsGlob (SP.fromRelDir [SP.reldir|server/src/a|]) `shouldBe` True
      match serverSrcDescendantsGlob "server/srcfoo/a.ts" `shouldBe` False

    it "can extend descendantsGlob with a file extension" $ do
      let tsFilesGlob = descendantsGlob [SP.reldir|src|] <> ".ts"
      match tsFilesGlob "src/a.ts" `shouldBe` True
      match tsFilesGlob "src/a/b.ts" `shouldBe` True
      match tsFilesGlob "src/a.css" `shouldBe` False
