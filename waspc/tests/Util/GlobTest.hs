module Util.GlobTest where

import qualified System.FilePath as FP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.Glob (dirAndDescendantsGlobs, recursiveFileGlobsWithExtensions)

spec_globConstruction :: Spec
spec_globConstruction =
  describe "glob construction" $ do
    it "builds recursive file globs for extensions" $ do
      recursiveFileGlobsWithExtensions "src" [".ts", ".js"]
        `shouldBe` ["src" FP.</> "**" FP.</> "*.ts", "src" FP.</> "**" FP.</> "*.js"]

    it "builds exact and nested directory globs" $ do
      let serverSrc = "server" FP.</> "src"
      dirAndDescendantsGlobs serverSrc
        `shouldBe` [serverSrc, serverSrc FP.</> "**" FP.</> "*"]
