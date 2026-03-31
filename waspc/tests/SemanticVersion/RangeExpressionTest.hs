module SemanticVersion.RangeExpressionTest where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.RangeExpression
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "show" $ do
    it "shows single simple range expression comparator set" $ do
      show
        ( Simple $
            NE.fromList
              [ Primitive Equal [pv|1.2|]
              ]
        )
        `shouldBe` "1.2"
    it "shows multiple simple range expression comparator set" $ do
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
    it "shows hyphen range" $ do
      show (HyphenRange [pv|1.2.3|] [pv|4.5.6|]) `shouldBe` "1.2.3 - 4.5.6"
      show (HyphenRange [pv|1.2|] [pv|3|]) `shouldBe` "1.2 - 3"
      show (HyphenRange [pv|1|] [pv|2.3|]) `shouldBe` "1 - 2.3"
      show (HyphenRange [pv|*|] [pv|*|]) `shouldBe` "* - *"

  describe "simpleRangeExpressionParser" $ do
    let parseSimple = P.parse simpleRangeExpressionParser ""
        strictParseSimple = P.parse (simpleRangeExpressionParser <* P.eof) ""

    it "parses primitive comparators" $ do
      strictParseSimple ">=1.2.3" `shouldBe` Right (Primitive GreaterThanOrEqual [pv|1.2.3|])
      strictParseSimple "<=1.2.3" `shouldBe` Right (Primitive LessThanOrEqual [pv|1.2.3|])
      strictParseSimple ">1.2.3" `shouldBe` Right (Primitive GreaterThan [pv|1.2.3|])
      strictParseSimple "<1.2.3" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])
      strictParseSimple "=1.2.3" `shouldBe` Right (Primitive Equal [pv|1.2.3|])

    it "parses caret comparators" $ do
      strictParseSimple "^1.2.3" `shouldBe` Right (CaretRange [pv|1.2.3|])

    it "parses tilde comparators" $ do
      strictParseSimple "~1.2.3" `shouldBe` Right (TildeRange [pv|1.2.3|])

    it "parses simple comparators with trailing content" $ do
      parseSimple "* 1.2.3" `shouldBe` Right (Primitive Equal [pv|*|])
      parseSimple "<1.2.3 || 5" `shouldBe` Right (Primitive LessThan [pv|1.2.3|])

    it "rejects invalid formats" $ do
      isLeft (strictParseSimple "") `shouldBe` True
      isLeft (strictParseSimple "foo") `shouldBe` True
      isLeft (strictParseSimple "$1.2.3") `shouldBe` True

  describe "hyphenRangeParser" $ do
    let parseHyphenRange = P.parse hyphenRangeParser ""
        strictParseHyphenRange = P.parse (hyphenRangeParser <* P.eof) ""

    it "parses hyphen range" $ do
      strictParseHyphenRange "1.2.3 - 1.2.3" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
      strictParseHyphenRange "1.2 - 1.2" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2|])
      strictParseHyphenRange "1 - 1" `shouldBe` Right (HyphenRange [pv|1|] [pv|1|])
      strictParseHyphenRange "* - *" `shouldBe` Right (HyphenRange [pv|*|] [pv|*|])

    it "parses hyphen range with trailing content" $ do
      parseHyphenRange "1.2.3 - 1.2.3 || something" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
      parseHyphenRange "1.2 - 1.2.3 ^1.2.3" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2.3|])

    it "rejects invalid formats" $ do
      isLeft (strictParseHyphenRange "") `shouldBe` True
      isLeft (strictParseHyphenRange "foo") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2 - ") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2 - a") `shouldBe` True
      -- It must be exactly " - " string between two versions
      isLeft (strictParseHyphenRange "1.2 -  1.2") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2  - 1.2") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2-1.2") `shouldBe` True

  describe "rangeExpressionParser" $ do
    let parseCompSet = P.parse rangeExpressionParser ""
        strictParseCompSet = P.parse (rangeExpressionParser <* P.eof) ""

    it "parses comparator sets with single comparator" $ do
      strictParseCompSet ">=1.2.3"
        `shouldBe` Right
          ( Simple $
              NE.fromList
                [ Primitive GreaterThanOrEqual [pv|1.2.3|]
                ]
          )
      strictParseCompSet "1 - 3" `shouldBe` Right (HyphenRange [pv|1|] [pv|3|])

    it "parses comparator sets with multiple comparators" $ do
      strictParseCompSet ">=1.2.3 <1.2.3"
        `shouldBe` Right
          ( Simple $
              NE.fromList
                [ Primitive GreaterThanOrEqual [pv|1.2.3|],
                  Primitive LessThan [pv|1.2.3|]
                ]
          )
      strictParseCompSet ">1.2.3    <=1.2.3      ^1.2  * ~0.1.X"
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

    it "parses comparator sets with trailing content" $ do
      parseCompSet ">=1.2.3 <1.2.3 || 1"
        `shouldBe` Right
          ( Simple $
              NE.fromList
                [ Primitive GreaterThanOrEqual [pv|1.2.3|],
                  Primitive LessThan [pv|1.2.3|]
                ]
          )
      parseCompSet ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
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

    it "rejects invalid formats" $ do
      isLeft (strictParseCompSet "") `shouldBe` True
      isLeft (strictParseCompSet "foo") `shouldBe` True
      isLeft (strictParseCompSet ">1<2") `shouldBe` True

    describe "hyphen ranges cannot be combined with other comparators" $ do
      it "parses hyphen ranges as sole comparator in a set" $ do
        isRight (strictParseCompSet "1.2.3 - 2.0.0") `shouldBe` True
        isRight (strictParseCompSet "1 - 3") `shouldBe` True

      it "rejects comparator sets that mix a hyphen range with other comparators" $ do
        isLeft (strictParseCompSet ">=1.2.3 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet ">1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "<2.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 >=3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 <3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "^1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "~1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 ^3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.x 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 3.x") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 1.2.3 - 2.0.0") `shouldBe` True

  describe "versionBounds Simple" $ do
    let simple ~> expectedInterval =
          it (show simple) $ versionBounds simple `shouldBe` expectedInterval

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

  describe "versionBounds HyphenRange" $ do
    let range ~> expectedInterval =
          it (show range) $ versionBounds range `shouldBe` expectedInterval

    -- Hyphen range bounds
    HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
    HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
    HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
    HyphenRange [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
    HyphenRange [pv|*|] [pv|*|] ~> allVersionsInterval

  -- Just does 'intervalIntersection' under the hood.
  describe "versionBounds RangeExpression" $ do
    let compSet ~> expectedInterval =
          it (show compSet) $ versionBounds compSet `shouldBe` expectedInterval
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
    -- Three comparators: the tightest wins on each side
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
