module Parser.JsImportTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)
import Path (relfile)

import Parser.Common (runWaspParser)
import Parser.JsImport (jsImport)
import qualified Wasp


spec_parseJsImport :: Spec
spec_parseJsImport = do
    it "Parses typical js import correctly" $ do
        runWaspParser jsImport "import something from \"some/file.js\""
            `shouldBe` Right (Wasp.JsImport "something" [relfile|some/file.js|])

    it "Parses correctly when there is whitespace up front" $ do
        runWaspParser jsImport " import something from \"some/file.js\""
            `shouldBe` Right (Wasp.JsImport "something" [relfile|some/file.js|])

    it "Parses correctly when 'from' is part of WHAT part" $ do
        runWaspParser jsImport "import somethingfrom from \"some/file.js\""
            `shouldBe` Right (Wasp.JsImport "somethingfrom" [relfile|some/file.js|])

    it "Throws error if there is no whitespace after import" $ do
        isLeft (runWaspParser jsImport "importsomething from \"some/file.js\"")
            `shouldBe` True

    it "For now we don't support single quotes in FROM part (TODO: support them in the future!)" $ do
        isLeft (runWaspParser jsImport "import something from 'some/file.js'")
            `shouldBe` True
