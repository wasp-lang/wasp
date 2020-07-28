module Parser.StyleTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)
import qualified Path as P

import qualified StrongPath as SP
import Parser.Common (runWaspParser)
import Parser.Style (style)
import qualified Wasp.Style


spec_parseStyle :: Spec
spec_parseStyle = do
    it "Parses external code file path correctly" $ do
        runWaspParser style "\"@ext/some/file.js\""
            `shouldBe` Right (Wasp.Style.ExtCodeCssFile $ SP.fromPathRelFile [P.relfile|some/file.js|])

    it "Parses css closure correctly" $ do
        runWaspParser style "{=css Some css code css=}"
            `shouldBe` Right (Wasp.Style.CssCode "Some css code")

    it "Throws error if path is not external code path." $ do
        isLeft (runWaspParser style "\"some/file.js\"")
            `shouldBe` True
