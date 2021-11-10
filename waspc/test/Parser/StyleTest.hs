module Parser.StyleTest where

import Data.Either (isLeft)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Parser.Common (runWaspParser)
import Wasp.Parser.Style (style)
import qualified Wasp.Wasp.Style as Wasp.Style

spec_parseStyle :: Spec
spec_parseStyle = do
  it "Parses external code file path correctly" $ do
    runWaspParser style "\"@ext/some/file.css\""
      `shouldBe` Right (Wasp.Style.ExtCodeCssFile [SP.relfileP|some/file.css|])

  it "Parses css closure correctly" $ do
    runWaspParser style "{=css Some css code css=}"
      `shouldBe` Right (Wasp.Style.CssCode "Some css code")

  it "Throws error if path is not external code path." $ do
    isLeft (runWaspParser style "\"some/file.css\"")
      `shouldBe` True
