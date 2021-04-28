module Parser.ExternalCodeTest where

import Data.Either (isLeft)
import Parser.Common (runWaspParser)
import Parser.ExternalCode (extCodeFilePathString)
import qualified Path.Posix as PPosix
import qualified StrongPath as SP
import Test.Tasty.Hspec

spec_ParserExternalCode :: Spec
spec_ParserExternalCode = do
  describe "Parsing external code file path string" $ do
    it "Correctly parses external code path in double quotes" $ do
      runWaspParser extCodeFilePathString "\"@ext/foo/bar.txt\""
        `shouldBe` Right (SP.fromPathRelFileP [PPosix.relfile|foo/bar.txt|])

    it "When path does not start with @ext/, returns Left" $ do
      isLeft (runWaspParser extCodeFilePathString "\"@ext2/foo/bar.txt\"") `shouldBe` True
