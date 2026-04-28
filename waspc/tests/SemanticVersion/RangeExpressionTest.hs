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
      it "SimpleRangeExpressionSet" $ do
        show
          ( SimpleRangeExpressionSet $
              NE.fromList
                [ PrimitiveRangeExpression Equal [pv|1.2|]
                ]
          )
          `shouldBe` "1.2"
        show
          ( SimpleRangeExpressionSet $
              NE.fromList
                [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.0|],
                  PrimitiveRangeExpression LessThan [pv|2.0.0|],
                  TildeRangeExpression [pv|1.2.3|],
                  CaretRangeExpression [pv|2.1.3|]
                ]
          )
          `shouldBe` ">=1.2.0 <2.0.0 ~1.2.3 ^2.1.3"
      it "HyphenRangeExpression" $ do
        show (HyphenRangeExpression [pv|1.2.3|] [pv|4.5.6|]) `shouldBe` "1.2.3 - 4.5.6"
        show (HyphenRangeExpression [pv|1.2|] [pv|3|]) `shouldBe` "1.2 - 3"
        show (HyphenRangeExpression [pv|1|] [pv|2.3|]) `shouldBe` "1 - 2.3"
        show (HyphenRangeExpression [pv|*|] [pv|*|]) `shouldBe` "* - *"

    describe "rangeExpressionParser" $ do
      let looseRangeExpressionParser = P.parse rangeExpressionParser ""
          strictRangeExpressionParser = P.parse (rangeExpressionParser <* P.eof) ""

      it "parses empty input correctly" $
        strictRangeExpressionParser "" `shouldBe` Right (SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression Equal Any)

      it "parses range expression with single simple range expression" $ do
        strictRangeExpressionParser ">=1.2.3"
          `shouldBe` Right
            ( SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|]
                  ]
            )
        strictRangeExpressionParser "1 - 3" `shouldBe` Right (HyphenRangeExpression [pv|1|] [pv|3|])

      it "parses range expression with multiple simple range expressions" $ do
        strictRangeExpressionParser ">=1.2.3 <1.2.3"
          `shouldBe` Right
            ( SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|],
                    PrimitiveRangeExpression LessThan [pv|1.2.3|]
                  ]
            )
        strictRangeExpressionParser ">1.2.3    <=1.2.3      ^1.2  * ~0.1.X"
          `shouldBe` Right
            ( SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThan [pv|1.2.3|],
                    PrimitiveRangeExpression LessThanOrEqual [pv|1.2.3|],
                    CaretRangeExpression [pv|1.2|],
                    PrimitiveRangeExpression Equal [pv|*|],
                    TildeRangeExpression [pv|0.1.X|]
                  ]
            )

      it "parses range with trailing content" $ do
        looseRangeExpressionParser ">=1.2.3 <1.2.3 || 1"
          `shouldBe` Right
            ( SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|],
                    PrimitiveRangeExpression LessThan [pv|1.2.3|]
                  ]
            )
        looseRangeExpressionParser ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
          `shouldBe` Right
            ( SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThan [pv|1.0.0|],
                    PrimitiveRangeExpression LessThanOrEqual [pv|2.0.0|],
                    CaretRangeExpression [pv|1.2|],
                    PrimitiveRangeExpression Equal [pv|*|],
                    TildeRangeExpression [pv|0.1.X|]
                  ]
            )

      it "rejects range expressions that mix a hyphen range expression with simple ranges expressions" $ do
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

    describe "hyphenRangeExpressionParser" $ do
      let looseParseHyphenRange = P.parse hyphenRangeExpressionParser ""
          strictParseHyphenRange = P.parse (hyphenRangeExpressionParser <* P.eof) ""

      it "parses hyphen range expression" $ do
        strictParseHyphenRange "1.2.3 - 1.2.3" `shouldBe` Right (HyphenRangeExpression [pv|1.2.3|] [pv|1.2.3|])
        strictParseHyphenRange "1.2 - 1.2" `shouldBe` Right (HyphenRangeExpression [pv|1.2|] [pv|1.2|])
        strictParseHyphenRange "1 - 1" `shouldBe` Right (HyphenRangeExpression [pv|1|] [pv|1|])
        strictParseHyphenRange "* - *" `shouldBe` Right (HyphenRangeExpression [pv|*|] [pv|*|])

      it "parses hyphen range expression with trailing content" $ do
        looseParseHyphenRange "1.2.3 - 1.2.3 || something" `shouldBe` Right (HyphenRangeExpression [pv|1.2.3|] [pv|1.2.3|])
        looseParseHyphenRange "1.2 - 1.2.3 ^1.2.3" `shouldBe` Right (HyphenRangeExpression [pv|1.2|] [pv|1.2.3|])

      it "rejects hyphen range expressions which don't have exact \" - \" string between two partial versions" $ do
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
      let rangeExpression ~> expectedInterval =
            it (show rangeExpression) $ versionBounds rangeExpression `shouldBe` expectedInterval

      -- SimpleRangeExpressionSet bounds
      -- NOTE: Just does 'intervalIntersection' under the hood.
      -- Basic inclusive/exclusive combinations
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.0.0|],
            PrimitiveRangeExpression LessThan [pv|2.0.0|]
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression GreaterThan [pv|1.0.0|],
            PrimitiveRangeExpression LessThanOrEqual [pv|2.0.0|]
          ]
        )
        ~> [vi| (1.0.0, 2.0.0] |]
      -- Caret narrows an open upper bound
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression GreaterThanOrEqual [pv|1.0.0|],
            CaretRangeExpression [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 2.0.0) |]
      -- Tilde narrows further than caret
      ( SimpleRangeExpressionSet . NE.fromList $
          [ CaretRangeExpression [pv|1.0.0|],
            TildeRangeExpression [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 1.3.0) |]
      -- Three range expressions: the tightest wins on each side
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression GreaterThan [pv|0.5.0|],
            PrimitiveRangeExpression LessThan [pv|3.0.0|],
            CaretRangeExpression [pv|1.0.0|]
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      -- Exclusive beats inclusive at the same version
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression GreaterThan [pv|1.0.0|],
            PrimitiveRangeExpression GreaterThanOrEqual [pv|1.0.0|]
          ]
        )
        ~> [vi| (1.0.0, inf) |]
      ( SimpleRangeExpressionSet . NE.fromList $
          [ PrimitiveRangeExpression LessThan [pv|2.0.0|],
            PrimitiveRangeExpression LessThanOrEqual [pv|2.0.0|]
          ]
        )
        ~> [vi| [0.0.0, 2.0.0) |]

      -- HyphenRangeExpression bounds
      HyphenRangeExpression [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
      HyphenRangeExpression [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
      HyphenRangeExpression [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
      HyphenRangeExpression [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
      HyphenRangeExpression [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
      HyphenRangeExpression [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
      HyphenRangeExpression [pv|*|] [pv|*|] ~> allVersionsInterval

  describe "SimpleRangeExpression" $ do
    it "show" $ do
      show (PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (PrimitiveRangeExpression GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (PrimitiveRangeExpression LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (PrimitiveRangeExpression LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (PrimitiveRangeExpression Equal [pv|1.2.3|]) `shouldBe` "1.2.3"
      show (TildeRangeExpression [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (CaretRangeExpression [pv|1.2.3|]) `shouldBe` "^1.2.3"

    describe "simpleRangeExpressionParser" $ do
      let looseParseSimpleRange = P.parse simpleRangeExpressionParser ""
          strictParseSimpleRange = P.parse (simpleRangeExpressionParser <* P.eof) ""

      it "parses primitive range expression" $ do
        strictParseSimpleRange ">=1.2.3" `shouldBe` Right (PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange "<=1.2.3" `shouldBe` Right (PrimitiveRangeExpression LessThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange ">1.2.3" `shouldBe` Right (PrimitiveRangeExpression GreaterThan [pv|1.2.3|])
        strictParseSimpleRange "<1.2.3" `shouldBe` Right (PrimitiveRangeExpression LessThan [pv|1.2.3|])
        strictParseSimpleRange "=1.2.3" `shouldBe` Right (PrimitiveRangeExpression Equal [pv|1.2.3|])

      it "parses implicit equal (no \"=\" operator)" $ do
        strictParseSimpleRange "1.2.3" `shouldBe` Right (PrimitiveRangeExpression Equal [pv|1.2.3|])

      it "parses caret range expression" $ do
        strictParseSimpleRange "^1.2.3" `shouldBe` Right (CaretRangeExpression [pv|1.2.3|])

      it "parses tilde range expression" $ do
        strictParseSimpleRange "~1.2.3" `shouldBe` Right (TildeRangeExpression [pv|1.2.3|])

      it "parses simple range expressions with whitespace between the partial version and operator" $ do
        strictParseSimpleRange ">=  1.2.3" `shouldBe` Right (PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange "<= 1.2.3" `shouldBe` Right (PrimitiveRangeExpression LessThanOrEqual [pv|1.2.3|])
        strictParseSimpleRange ">  1.2.3" `shouldBe` Right (PrimitiveRangeExpression GreaterThan [pv|1.2.3|])
        strictParseSimpleRange "<    1.2.3" `shouldBe` Right (PrimitiveRangeExpression LessThan [pv|1.2.3|])
        strictParseSimpleRange "=  1.2.3" `shouldBe` Right (PrimitiveRangeExpression Equal [pv|1.2.3|])
        strictParseSimpleRange "^  1.2.3" `shouldBe` Right (CaretRangeExpression [pv|1.2.3|])
        strictParseSimpleRange "~ 1.2.3" `shouldBe` Right (TildeRangeExpression [pv|1.2.3|])

      it "parses simple range expression with trailing content" $ do
        looseParseSimpleRange "* 1.2.3" `shouldBe` Right (PrimitiveRangeExpression Equal [pv|*|])
        looseParseSimpleRange "<1.2.3 || 5" `shouldBe` Right (PrimitiveRangeExpression LessThan [pv|1.2.3|])
        looseParseSimpleRange "<1.2.3 a 5" `shouldBe` Right (PrimitiveRangeExpression LessThan [pv|1.2.3|])

      it "rejects invalid formats" $ do
        isLeft (strictParseSimpleRange "") `shouldBe` True
        isLeft (strictParseSimpleRange "foo") `shouldBe` True
        isLeft (strictParseSimpleRange "$1.2.3") `shouldBe` True
        isLeft (strictParseSimpleRange "?1.x.x") `shouldBe` True

    describe "versionBounds" $ do
      let simpleRangeExpression ~> expectedInterval =
            it (show simpleRangeExpression) $ versionBounds simpleRangeExpression `shouldBe` expectedInterval

      -- Equal
      PrimitiveRangeExpression Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
      PrimitiveRangeExpression Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      PrimitiveRangeExpression Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      PrimitiveRangeExpression Equal [pv|*|] ~> allVersionsInterval

      -- GreaterThan
      PrimitiveRangeExpression GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
      PrimitiveRangeExpression GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
      PrimitiveRangeExpression GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]
      PrimitiveRangeExpression GreaterThan [pv|*|] ~> noVersionInterval

      -- LessThan
      PrimitiveRangeExpression LessThan [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3) |]
      PrimitiveRangeExpression LessThan [pv|1.2|] ~> [vi| [0.0.0, 1.2.0) |]
      PrimitiveRangeExpression LessThan [pv|1|] ~> [vi| [0.0.0, 1.0.0) |]
      PrimitiveRangeExpression LessThan [pv|*|] ~> noVersionInterval

      -- GreaterThanOrEqual
      PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
      PrimitiveRangeExpression GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
      PrimitiveRangeExpression GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]
      PrimitiveRangeExpression GreaterThanOrEqual [pv|*|] ~> allVersionsInterval

      -- LessThanOrEqual
      PrimitiveRangeExpression LessThanOrEqual [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
      PrimitiveRangeExpression LessThanOrEqual [pv|1.2|] ~> [vi| [0.0.0, 1.3.0) |]
      PrimitiveRangeExpression LessThanOrEqual [pv|1|] ~> [vi| [0.0.0, 2.0.0) |]
      PrimitiveRangeExpression LessThanOrEqual [pv|*|] ~> allVersionsInterval

      -- Tilde range bounds
      TildeRangeExpression [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
      TildeRangeExpression [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      TildeRangeExpression [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      TildeRangeExpression [pv|*|] ~> allVersionsInterval

      -- Caret range bounds
      CaretRangeExpression [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
      CaretRangeExpression [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
      CaretRangeExpression [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
      CaretRangeExpression [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
      CaretRangeExpression [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
      CaretRangeExpression [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
      CaretRangeExpression [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      CaretRangeExpression [pv|0|] ~> [vi| [0.0.0, 1.0.0) |]
      CaretRangeExpression [pv|*|] ~> allVersionsInterval
