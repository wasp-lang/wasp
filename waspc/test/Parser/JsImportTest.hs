module Parser.JsImportTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)
import qualified Path as P

import qualified StrongPath as SP
import Parser.Common (runWaspParser)
import Parser.JsImport (jsImport)
import qualified Wasp


spec_parseJsImport :: Spec
spec_parseJsImport = do
    it "Parses external code js import with default import correctly" $ do
        runWaspParser jsImport "import something from \"@ext/some/file.js\""
            `shouldBe` Right (Wasp.JsImport "something" (SP.fromPathRelFile [P.relfile|some/file.js|]))

    it "Parses correctly when there is whitespace up front" $ do
        runWaspParser jsImport " import something from \"@ext/some/file.js\""
            `shouldBe` Right (Wasp.JsImport "something" (SP.fromPathRelFile [P.relfile|some/file.js|]))

    it "Parses correctly when 'from' is part of WHAT part" $ do
        runWaspParser jsImport "import somethingfrom from \"@ext/some/file.js\""
            `shouldBe` Right (Wasp.JsImport "somethingfrom" (SP.fromPathRelFile [P.relfile|some/file.js|]))

    it "Parses correctly when 'what' is a single named export" $ do
        runWaspParser jsImport "import { something } from \"@ext/some/file.js\""
            `shouldBe` Right (Wasp.JsImport "something" (SP.fromPathRelFile [P.relfile|some/file.js|]))

    it "For now we don't support multiple named exports in WHAT part" $ do
        isLeft (runWaspParser jsImport "import { foo, bar } from \"@ext/some/file.js\"")
            `shouldBe` True

    it "Throws error if there is no whitespace after import" $ do
        isLeft (runWaspParser jsImport "importsomething from \"@ext/some/file.js\"")
            `shouldBe` True

    it "Throws error if 'from' part is not referring to external code" $ do
        isLeft (runWaspParser jsImport "import something from \"some/file.js\"")
            `shouldBe` True

    it "For now we don't support single quotes in FROM part (TODO: support them in the future!)" $ do
        isLeft (runWaspParser jsImport "import something from '@ext/some/file.js'")
            `shouldBe` True
