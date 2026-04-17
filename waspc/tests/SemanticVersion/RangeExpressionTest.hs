module SemanticVersion.RangeExpressionTest where

import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.RangeExpression
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_RangeExpression :: Spec
spec_SemanticVersion_RangeExpression = do
  describe "RangeExpression" $ do
    describe "show" $ do
      it "Simple" $ do
        show
          ( Simple $
              NE.fromList
                [ Primitive Equal [pv|1.2|]
                ]
          )
          `shouldBe` "1.2"
        show
          ( Simple $
              NE.fromList
                [ Primitive GreaterThanOrEqual [pv|1.2.0|],
                  Primitive LessThan [pv|2.0.0|],
                  TildeRange [pv|1.2.3|],
                  CaretRange [pv|2.1.3|]
                ]
          )
          `shouldBe` ">=1.2.0 <2.0.0 ~1.2.3 ^2.1.3"
      it "HyphenRange" $ do
        show (HyphenRange [pv|1.2.3|] [pv|4.5.6|]) `shouldBe` "1.2.3 - 4.5.6"
        show (HyphenRange [pv|1.2|] [pv|3|]) `shouldBe` "1.2 - 3"
        show (HyphenRange [pv|1|] [pv|2.3|]) `shouldBe` "1 - 2.3"
        show (HyphenRange [pv|*|] [pv|*|]) `shouldBe` "* - *"

    describe "rangeExpressionParser" $ do
      let looseRangeExpressionParser = P.parse rangeExpressionParser ""
          strictRangeExpressionParser = P.parse (rangeExpressionParser <* P.eof) ""

      it "parses empty input correctly" $
        strictRangeExpressionParser "" `shouldBe` Right (Simple $ pure $ Primitive Equal Any)

      it "parses range expression with single simple range" $ do
        strictRangeExpressionParser ">=1.2.3"
          `shouldBe` Right
            ( Simple $
                NE.fromList
                  [ Primitive GreaterThanOrEqual [pv|1.2.3|]
                  ]
            )
        strictRangeExpressionParser "1 - 3" `shouldBe` Right (HyphenRange [pv|1|] [pv|3|])

      it "parses range with multiple simple range" $ do
        strictRangeExpressionParser ">=1.2.3 <1.2.3"
          `shouldBe` Right
            ( Simple $
                NE.fromList
                  [ Primitive GreaterThanOrEqual [pv|1.2.3|],
                    Primitive LessThan [pv|1.2.3|]
                  ]
            )
        strictRangeExpressionParser ">1.2.3    <=1.2.3      ^1.2  * ~0.1.X"
          `shouldBe` Right
            ( Simple $
                NE.fromList
                  [ Primitive GreaterThan [pv|1.2.3|],
                    Primitive LessThanOrEqual [pv|1.2.3|],
                    CaretRange [pv|1.2|],
                    Primitive Equal [pv|*|],
                    TildeRange [pv|0.1.X|]
                  ]
            )

      it "parses range with trailing content" $ do
        looseRangeExpressionParser ">=1.2.3 <1.2.3 || 1"
          `shouldBe` Right
            ( Simple $
                NE.fromList
                  [ Primitive GreaterThanOrEqual [pv|1.2.3|],
                    Primitive LessThan [pv|1.2.3|]
                  ]
            )
        looseRangeExpressionParser ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
          `shouldBe` Right
            ( Simple $
                NE.fromList
                  [ Primitive GreaterThan [pv|1.0.0|],
                    Primitive LessThanOrEqual [pv|2.0.0|],
                    CaretRange [pv|1.2|],
                    Primitive Equal [pv|*|],
                    TildeRange [pv|0.1.X|]
                  ]
            )

      it "rejects range expressions that mix a hyphen range with simple ranges" $ do
        isLeft (strictRangeExpressionParser ">=1.2.3 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser ">1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "<2.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.2.3 - 2.0.0 >=3.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.2.3 - 2.0.0 <3.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "^1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "~1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.2.3 - 2.0.0 ^3.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.x 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.2.3 - 2.0.0 3.x") `shouldBe` True
        isLeft (strictRangeExpressionParser "1.2.3 - 2.0.0 1.2.3 - 2.0.0") `shouldBe` True

      it "rejects invalid formats" $ do
        isLeft (strictRangeExpressionParser "foo") `shouldBe` True
        isLeft (strictRangeExpressionParser ">1<2") `shouldBe` True

    describe "hyphenRangeParser" $ do
      let looseParseHyphenRange = P.parse hyphenRangeParser ""
          strictParseHyphenRange = P.parse (hyphenRangeParser <* P.eof) ""

      it "parses hyphen range" $ do
        strictParseHyphenRange "1.2.3 - 1.2.3" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
        strictParseHyphenRange "1.2 - 1.2" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2|])
        strictParseHyphenRange "1 - 1" `shouldBe` Right (HyphenRange [pv|1|] [pv|1|])
        strictParseHyphenRange "* - *" `shouldBe` Right (HyphenRange [pv|*|] [pv|*|])

      it "parses hyphen range with trailing content" $ do
        looseParseHyphenRange "1.2.3 - 1.2.3 || something" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
        looseParseHyphenRange "1.2 - 1.2.3 ^1.2.3" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2.3|])

      it "rejects hyphen ranges which don't have exact \" - \" string between two partial versions" $ do
        isLeft (strictParseHyphenRange "1.2 -  1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2  - 1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2-1.2") `shouldBe` True

      it "rejects invalid formats" $ do
        isLeft (strictParseHyphenRange "") `shouldBe` True
        isLeft (strictParseHyphenRange "foo") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2 - ") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2 - a") `shouldBe` True

    describe "versionBounds" $ do
      let simple ~> expectedInterval =
            it (show simple) $ versionBounds simple `shouldBe` expectedInterval

      -- Simple bounds
      -- Just does 'intervalIntersection' under the hood.

      -- Basic inclusive/exclusive combinations
      ( Simple . NE.fromList $
          [ Primitive GreaterThanOrEqual [pv|1.0.0|],
            Primitive LessThan [pv|2.0.0|]
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      ( Simple . NE.fromList $
          [ Primitive GreaterThan [pv|1.0.0|],
            Primitive LessThanOrEqual [pv|2.0.0|]
          ]
        )
        ~> [vi| (1.0.0, 2.0.0] |]
      -- Caret narrows an open upper bound
      ( Simple . NE.fromList $
          [ Primitive GreaterThanOrEqual [pv|1.0.0|],
            CaretRange [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 2.0.0) |]
      -- Tilde narrows further than caret
      ( Simple . NE.fromList $
          [ CaretRange [pv|1.0.0|],
            TildeRange [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 1.3.0) |]
      -- Three range expressions: the tightest wins on each side
      ( Simple . NE.fromList $
          [ Primitive GreaterThan [pv|0.5.0|],
            Primitive LessThan [pv|3.0.0|],
            CaretRange [pv|1.0.0|]
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      -- Exclusive beats inclusive at the same version
      ( Simple . NE.fromList $
          [ Primitive GreaterThan [pv|1.0.0|],
            Primitive GreaterThanOrEqual [pv|1.0.0|]
          ]
        )
        ~> [vi| (1.0.0, inf) |]
      ( Simple . NE.fromList $
          [ Primitive LessThan [pv|2.0.0|],
            Primitive LessThanOrEqual [pv|2.0.0|]
          ]
        )
        ~> [vi| [0.0.0, 2.0.0) |]

      -- HyphenRange bounds
      HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
      HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
      HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
      HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
      HyphenRange [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
      HyphenRange [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
      HyphenRange [pv|*|] [pv|*|] ~> allVersionsInterval

  describe "SimpleRangeExpression" $ do
    it "show" $ do
      show (Primitive GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (Primitive GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (Primitive LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (Primitive LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (Primitive Equal [pv|1.2.3|]) `shouldBe` "1.2.3"
      show (TildeRange [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (CaretRange [pv|1.2.3|]) `shouldBe` "^1.2.3"

    describe "simpleRangeParser" $ do
      let looseParseSimpleRange = P.parse simpleRangeParser ""
          strictParseSimpleRange = P.parse (simpleRangeParser <* P.eof) ""

      it "parses primitive range expression" $ do
        strictParseSimpleRange ">=1.2.3" `shouldBe` Right (Primitive GreaterThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange "<=1.2.3" `shouldBe` Right (Primitive LessThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange ">1.2.3" `shouldBe` Right (Primitive GreaterThan [pv|1.2.3|])
        strictParseSimpleRange "<1.2.3" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])
        strictParseSimpleRange "=1.2.3" `shouldBe` Right (Primitive Equal [pv|1.2.3|])

      it "parses implicit equal (no \"=\" operator)" $ do
        strictParseSimpleRange "1.2.3" `shouldBe` Right (Primitive Equal [pv|1.2.3|])

      it "parses caret range expression" $ do
        strictParseSimpleRange "^1.2.3" `shouldBe` Right (CaretRange [pv|1.2.3|])

      it "parses tilde range expression" $ do
        strictParseSimpleRange "~1.2.3" `shouldBe` Right (TildeRange [pv|1.2.3|])

      it "parses simple range expressions with whitespace between the partial version and operator" $ do
        strictParseSimpleRange ">=  1.2.3" `shouldBe` Right (Primitive GreaterThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange "<= 1.2.3" `shouldBe` Right (Primitive LessThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange ">  1.2.3" `shouldBe` Right (Primitive GreaterThan [pv|1.2.3|])
        strictParseSimpleRange "<    1.2.3" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])
        strictParseSimpleRange "=  1.2.3" `shouldBe` Right (Primitive Equal [pv|1.2.3|])
        strictParseSimpleRange "^  1.2.3" `shouldBe` Right (CaretRange [pv|1.2.3|])
        strictParseSimpleRange "~ 1.2.3" `shouldBe` Right (TildeRange [pv|1.2.3|])

      it "parses simple range expression with trailing content" $ do
        looseParseSimpleRange "* 1.2.3" `shouldBe` Right (Primitive Equal [pv|*|])
        looseParseSimpleRange "<1.2.3 || 5" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])
        looseParseSimpleRange "<1.2.3 a 5" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])

      it "rejects invalid formats" $ do
        isLeft (strictParseSimpleRange "") `shouldBe` True
        isLeft (strictParseSimpleRange "foo") `shouldBe` True
        isLeft (strictParseSimpleRange "$1.2.3") `shouldBe` True
        isLeft (strictParseSimpleRange "?1.x.x") `shouldBe` True

    describe "versionBounds" $ do
      let simple ~> expectedInterval =
            it (show simple) $ versionBounds simple `shouldBe` expectedInterval

      -- Equal
      Primitive Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
      Primitive Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      Primitive Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      Primitive Equal [pv|*|] ~> allVersionsInterval

      -- GreaterThan
      Primitive GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
      Primitive GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
      Primitive GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]
      Primitive GreaterThan [pv|*|] ~> noVersionInterval

      -- LessThan
      Primitive LessThan [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3) |]
      Primitive LessThan [pv|1.2|] ~> [vi| [0.0.0, 1.2.0) |]
      Primitive LessThan [pv|1|] ~> [vi| [0.0.0, 1.0.0) |]
      Primitive LessThan [pv|*|] ~> noVersionInterval

      -- GreaterThanOrEqual
      Primitive GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
      Primitive GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
      Primitive GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]
      Primitive GreaterThanOrEqual [pv|*|] ~> allVersionsInterval

      -- LessThanOrEqual
      Primitive LessThanOrEqual [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
      Primitive LessThanOrEqual [pv|1.2|] ~> [vi| [0.0.0, 1.3.0) |]
      Primitive LessThanOrEqual [pv|1|] ~> [vi| [0.0.0, 2.0.0) |]
      Primitive LessThanOrEqual [pv|*|] ~> allVersionsInterval

      -- Tilde range bounds
      TildeRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
      TildeRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      TildeRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      TildeRange [pv|*|] ~> allVersionsInterval

      -- Caret range bounds
      CaretRange [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
      CaretRange [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
      CaretRange [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
      CaretRange [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
      CaretRange [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
      CaretRange [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
      CaretRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      CaretRange [pv|0|] ~> [vi| [0.0.0, 1.0.0) |]
      CaretRange [pv|*|] ~> allVersionsInterval