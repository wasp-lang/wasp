module Wasp.LSP.SyntaxTest where

import qualified Language.LSP.Types as LSP
import Test.QuickCheck
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (SourceSpan))
import Wasp.LSP.Syntax

spec_lspPositionToOffset :: Spec
spec_lspPositionToOffset = describe "Wasp.LSP.Syntax.lspPositionToOffset" $ do
  it "works for 1-line strings" $ do
    let src = "hello world"
    let pos = LSP.Position 0 5
    lspPositionToOffset src pos `shouldBe` 5

  it "works for a multiline string and counts \\n in the offset" $ do
    let src = "hello\nworld"
    let pos = LSP.Position 1 2
    lspPositionToOffset src pos `shouldBe` 8

  it "works for a string with many lines" $ do
    let srcLines = ["abc", "xyz", "ijk", "lmno", "wasp is the best", "123"]
    let src = unlines srcLines
    let pos = LSP.Position 4 3
    lspPositionToOffset src pos `shouldBe` 20

spec_lspRangeToSpan :: Spec
spec_lspRangeToSpan = describe "Wasp.LSP.Syntax.lspRangeToSpan" $ do
  it "is the same as running lspPositionToOffset on each endpoint of the range" $ do
    property $
      \src
       (lineStart :: Int)
       (colStart :: Int)
       (lineEnd :: Int)
       (colEnd :: Int) ->
          let start = LSP.Position (fromIntegral lineStart) (fromIntegral colStart)
              end = LSP.Position (fromIntegral lineEnd) (fromIntegral colEnd)
              range = LSP.Range start end
           in lspRangeToSpan src range == SourceSpan (lspPositionToOffset src start) (lspPositionToOffset src end)
