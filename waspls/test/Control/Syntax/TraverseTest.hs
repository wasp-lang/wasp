{-# LANGUAGE TypeFamilies #-}

module Control.Syntax.TraverseTest where

import Control.Syntax.Traverse
import Test.Tasty.Hspec (Spec, it, shouldBe)
import Test.Tasty.QuickCheck ()
import Wasp.Backend.ConcreteParser (parseCST)
import Wasp.Backend.ConcreteSyntax (SyntaxKind (..), SyntaxNode)
import qualified Wasp.Backend.Lexer as L
import qualified Wasp.Backend.Token as T

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
    let declName = root & down .> down .> right .> right
    kindAt declName `shouldBe` DeclName
    let dictKey = declName & right .> right .> down .> right .> right .> right .> down
    kindAt dictKey `shouldBe` DictKey

  it "next goes as deep as possible" $ do
    let start = fromSyntaxForest example1 & next
    kindAt start `shouldBe` DeclType

  it "Can traverse across a tree" $ do
    let start = fromSyntaxForest example1 & down .> down
    kindAt start `shouldBe` DeclType
    let end = start & next .> next .> next .> next
    kindAt end `shouldBe` Token T.LCurly
    let start' = end & back .> back .> back .> back
    kindAt start' `shouldBe` kindAt start

  it "Keeps track of offsets correctly" $ do
    let start = fromSyntaxForest example1 & next -- Get to first leaf token
    offsetAt start `shouldBe` 0
    -- Check offset at {
    kindAt (start & next .> next .> next .> next) `shouldBe` Token T.LCurly
    offsetAt (start & next .> next .> next .> next) `shouldBe` 9
    -- Check offset after moving around
    offsetAt (start & right .> left) `shouldBe` 0
    -- Check offset after moving up and down
    let dictStart = start & right .> right .> right .> right
    offsetAt (dictStart & down .> up) `shouldBe` offsetAt dictStart
