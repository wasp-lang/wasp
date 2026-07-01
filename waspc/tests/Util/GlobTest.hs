module Util.GlobTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.Glob (compileGlobPatterns, dirAndDescendantsGlobs, matchesAnyGlob, recursiveFileGlobsWithExtensions)

spec_GlobPatterns :: Spec
spec_GlobPatterns =
  describe "GlobPatterns" $ do
    it "matches files directly inside a directory" $ do
      let patterns = compileGlobPatterns ["src/**/*.ts"]
      patterns `matchesAnyGlob` "src/foo.ts" `shouldBe` True

    it "matches files nested inside a directory" $ do
      let patterns = compileGlobPatterns ["src/**/*.ts"]
      patterns `matchesAnyGlob` "src/actions/foo.ts" `shouldBe` True

    it "does not match a different file extension" $ do
      let patterns = compileGlobPatterns ["src/**/*.ts"]
      patterns `matchesAnyGlob` "src/actions/foo.css" `shouldBe` False

    it "matches if any pattern matches" $ do
      let patterns = compileGlobPatterns ["src/**/*.ts", "src/**/*.js"]
      patterns `matchesAnyGlob` "src/actions/foo.js" `shouldBe` True

    it "matches exact file patterns" $ do
      let patterns = compileGlobPatterns ["server/.env"]
      patterns `matchesAnyGlob` "server/.env" `shouldBe` True
      patterns `matchesAnyGlob` "server/src/.env" `shouldBe` False

    it "matches exact and nested directory paths" $ do
      let patterns = compileGlobPatterns ["server/src", "server/src/**"]
      patterns `matchesAnyGlob` "server/src" `shouldBe` True
      patterns `matchesAnyGlob` "server/src/routes" `shouldBe` True
      patterns `matchesAnyGlob` "server" `shouldBe` False

    it "builds recursive file globs for extensions" $ do
      recursiveFileGlobsWithExtensions "src" [".ts", ".js"]
        `shouldBe` ["src/**/*.ts", "src/**/*.js"]

    it "builds exact and nested directory globs" $ do
      dirAndDescendantsGlobs "server/src"
        `shouldBe` ["server/src", "server/src/**"]
