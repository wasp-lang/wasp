module Parser.ExternalCodeTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)
import qualified Path.Posix as PPosix

import Parser.ExternalCode (extCodeFilePathString)
import Parser.Common (runWaspParser)


spec_ParserExternalCode :: Spec
spec_ParserExternalCode = do
    describe "Parsing external code file path string" $ do
        it "Correctly parses external code path in double quotes" $ do
            runWaspParser extCodeFilePathString "\"@ext/foo/bar.txt\""
                `shouldBe` Right [PPosix.relfile|foo/bar.txt|]

        it "When path does not start with @ext/, returns Left" $ do
            isLeft (runWaspParser extCodeFilePathString "\"@ext2/foo/bar.txt\"") `shouldBe` True
