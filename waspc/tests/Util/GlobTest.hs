module Util.GlobTest where

import qualified System.FilePath as FP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.Glob (compileGlobPatterns, matchesAnyGlob)

spec_GlobPatterns :: Spec
spec_GlobPatterns =
  describe "GlobPatterns" $ do
    it "matches files directly inside a directory" $ do
      let patterns = compileGlobPatterns ["src" FP.</> "**" FP.</> "*.ts"]
      patterns `matchesAnyGlob` ("src" FP.</> "foo.ts") `shouldBe` True

    it "matches files nested inside a directory" $ do
      let patterns = compileGlobPatterns ["src" FP.</> "**" FP.</> "*.ts"]
      patterns `matchesAnyGlob` ("src" FP.</> "actions" FP.</> "foo.ts") `shouldBe` True

    it "does not match a different file extension" $ do
      let patterns = compileGlobPatterns ["src" FP.</> "**" FP.</> "*.ts"]
      patterns `matchesAnyGlob` ("src" FP.</> "actions" FP.</> "foo.css") `shouldBe` False

    it "matches if any pattern matches" $ do
      let patterns = compileGlobPatterns ["src" FP.</> "**" FP.</> "*.ts", "src" FP.</> "**" FP.</> "*.js"]
      patterns `matchesAnyGlob` ("src" FP.</> "actions" FP.</> "foo.js") `shouldBe` True

    it "matches exact file patterns" $ do
      let patterns = compileGlobPatterns ["server" FP.</> ".env"]
      patterns `matchesAnyGlob` ("server" FP.</> ".env") `shouldBe` True
      patterns `matchesAnyGlob` ("server" FP.</> "src" FP.</> ".env") `shouldBe` False

    it "matches exact and nested directory paths" $ do
      let patterns = compileGlobPatterns ["server" FP.</> "src", "server" FP.</> "src" FP.</> "**" FP.</> "*"]
      patterns `matchesAnyGlob` ("server" FP.</> "src") `shouldBe` True
      patterns `matchesAnyGlob` ("server" FP.</> "src" FP.</> "routes") `shouldBe` True
      patterns `matchesAnyGlob` ("server" FP.</> "src" FP.</> "routes" FP.</> "auth") `shouldBe` True
      patterns `matchesAnyGlob` "server" `shouldBe` False
