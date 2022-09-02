{-# LANGUAGE TypeFamilies #-}

module Analyzer.Parser.ConcreteParser.CST.TraverseTest where

import Test.Tasty.Hspec (Spec, it, shouldBe)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind (..), SyntaxNode)
import Wasp.Analyzer.Parser.ConcreteParser.CST.Traverse
import qualified Wasp.Analyzer.Parser.Lexer as L
import qualified Wasp.Analyzer.Parser.Token as T

example1 :: [SyntaxNode]
example1 =
  snd $
    parseCST $
      L.lex $
        unlines
          [ "app Main {",
            "  test: 5",
            "}"
          ]

spec_Traverse :: Spec
spec_Traverse = do
  it "Can traverse around a tree" $ do
    let root = fromSyntaxForest example1
    kindAt root `shouldBe` Program
    let declName = root & pipe [down, down, right, right]
    (kindAt <$> declName) `shouldBe` Just DeclName
    let dictKey = declName &? pipe [right, right, down, right, right, right, down]
    (kindAt <$> dictKey) `shouldBe` Just DictKey

  it "next goes as deep as possible" $ do
    let start = fromSyntaxForest example1 & next
    (kindAt <$> start) `shouldBe` Just DeclType

  it "Can traverse across a tree" $ do
    let start = fromSyntaxForest example1 & pipe [down, down]
    (kindAt <$> start) `shouldBe` Just DeclType
    let end = start &? pipe (replicate 4 next)
    (kindAt <$> end) `shouldBe` Just (Token T.LCurly)
    let start' = end &? pipe (replicate 4 previous)
    (kindAt <$> start') `shouldBe` (kindAt <$> start)

  it "Keeps track of offsets correctly" $ do
    let start = fromSyntaxForest example1 & next -- Get to first leaf token
    (offsetAt <$> start) `shouldBe` Just 0
    -- Check offset at {
    (kindAt <$> (start &? pipe (replicate 4 next))) `shouldBe` Just (Token T.LCurly)
    (offsetAt <$> (start &? pipe (replicate 4 next))) `shouldBe` Just 9
    -- Check offset after moving around
    (offsetAt <$> (start &? pipe [right, left])) `shouldBe` Just 0
    -- Check offset after moving up and down
    let dictStart = start &? pipe (replicate 4 right)
    (offsetAt <$> (dictStart &? pipe [down, up])) `shouldBe` (offsetAt <$> dictStart)
