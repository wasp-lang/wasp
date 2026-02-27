module SemanticVersion.Range.ParserTest where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_Range_Parser :: Spec
spec_SemanticVersion_Range_Parser = do
  describe "parseRange" $ do
    it "parses primitive operator ranges" $ do
      ">=1.2.3" `shouldParseTo` [primCompSet GreaterThanOrEqual (Full 1 2 3)]
      "<=1.2.3" `shouldParseTo` [primCompSet LessThanOrEqual (Full 1 2 3)]
      ">1.2.3" `shouldParseTo` [primCompSet GreaterThan (Full 1 2 3)]
      "<1.2.3" `shouldParseTo` [primCompSet LessThan (Full 1 2 3)]
      "=1.2.3" `shouldParseTo` [primCompSet Equal (Full 1 2 3)]
      ">=1.2" `shouldParseTo` [primCompSet GreaterThanOrEqual (MajorMinor 1 2)]
      "<=1.2" `shouldParseTo` [primCompSet LessThanOrEqual (MajorMinor 1 2)]
      "=1.2" `shouldParseTo` [primCompSet Equal (MajorMinor 1 2)]
      ">1" `shouldParseTo` [primCompSet GreaterThan (Major 1)]
      "<1" `shouldParseTo` [primCompSet LessThan (Major 1)]

    it "parses caret ranges" $ do
      "^1.2.3" `shouldParseTo` [compSet $ BackwardsCompatibleWith (Full 1 2 3)]
      "^1.2" `shouldParseTo` [compSet $ BackwardsCompatibleWith (MajorMinor 1 2)]
      "^1" `shouldParseTo` [compSet $ BackwardsCompatibleWith (Major 1)]

    it "parses approximatelyEquvivalentTo ranges" $ do
      "~1.2.3" `shouldParseTo` [compSet $ ApproximatelyEquvivalentTo (Full 1 2 3)]
      "~1.2" `shouldParseTo` [compSet $ ApproximatelyEquvivalentTo (MajorMinor 1 2)]
      "~1" `shouldParseTo` [compSet $ ApproximatelyEquvivalentTo (Major 1)]

    it "parses X-ranges" $ do
      "*" `shouldParseTo` [compSet $ XRange Any]
      "x" `shouldParseTo` [compSet $ XRange Any]
      "X" `shouldParseTo` [compSet $ XRange Any]
      "1" `shouldParseTo` [compSet $ XRange (Major 1)]
      "1.x" `shouldParseTo` [compSet $ XRange (Major 1)]
      "1.x.x" `shouldParseTo` [compSet $ XRange (Major 1)]
      "1.2" `shouldParseTo` [compSet $ XRange (MajorMinor 1 2)]
      "1.2.x" `shouldParseTo` [compSet $ XRange (MajorMinor 1 2)]
      "1.2.3" `shouldParseTo` [compSet $ XRange (Full 1 2 3)]

    it "parses hyphen ranges" $ do
      "1.2.3 - 2.3.4" `shouldParseTo` [compSet $ HyphenRange (Full 1 2 3) (Full 2 3 4)]
      "1.2 - 2.3.4" `shouldParseTo` [compSet $ HyphenRange (MajorMinor 1 2) (Full 2 3 4)]
      "1.2.3 - 2.3" `shouldParseTo` [compSet $ HyphenRange (Full 1 2 3) (MajorMinor 2 3)]
      "1 - 3" `shouldParseTo` [compSet $ HyphenRange (Major 1) (Major 3)]

    it "parses comparator sets with multiple comparators (AND)" $ do
      ">=1.0.0 <2.0.0"
        `shouldParseTo` [ ComparatorSet $
                            NE.fromList
                              [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                                PrimitiveComparator LessThan (Full 2 0 0)
                              ]
                        ]
      ">1.0.0 <=2.0.0"
        `shouldParseTo` [ ComparatorSet $
                            NE.fromList
                              [ PrimitiveComparator GreaterThan (Full 1 0 0),
                                PrimitiveComparator LessThanOrEqual (Full 2 0 0)
                              ]
                        ]

    it "parses ranges with multiple comparator sets (OR)" $ do
      ">=1.0.0 <2.0.0 || >=3.0.0"
        `shouldParseTo` [ ComparatorSet $
                            NE.fromList
                              [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                                PrimitiveComparator LessThan (Full 2 0 0)
                              ],
                          primCompSet GreaterThanOrEqual (Full 3 0 0)
                        ]
      "^1.2.3 || ^2.0.0"
        `shouldParseTo` [ compSet $ BackwardsCompatibleWith (Full 1 2 3),
                          compSet $ BackwardsCompatibleWith (Full 2 0 0)
                        ]

    -- Hyphen ranges are mutually exclusive with other comparators within the same comparator set.
    -- They can only appear alone in a comparator set, or combined with other sets via ||.
    describe "hyphen ranges cannot be combined with other comparators in the same comparator set" $ do
      it "parses hyphen ranges when they are the only comparator in their set" $ do
        shouldParse "1.2.3 - 2.0.0 || >=3.0.0"
        shouldParse ">=0.5.0 || 1.2.3 - 2.0.0"
        shouldParse "^1.0.0 || ~1.0.0 || 1.2.3 - 2.0.0"
        shouldParse "1.0.0 - 2.0.0 || 3.0.0 - 4.0.0"

      it "rejects comparator sets that mix a hyphen range with any other comparator" $ do
        shouldNotParse ">=1.2.3 1.2.3 - 2.0.0"
        shouldNotParse ">1.0.0 1.2.3 - 2.0.0"
        shouldNotParse "<2.0.0 1.2.3 - 2.0.0"
        shouldNotParse "1.2.3 - 2.0.0 >=3.0.0"
        shouldNotParse "1.2.3 - 2.0.0 <3.0.0"
        shouldNotParse "^1.0.0 1.2.3 - 2.0.0"
        shouldNotParse "~1.0.0 1.2.3 - 2.0.0"
        shouldNotParse "1.2.3 - 2.0.0 ^3.0.0"
        shouldNotParse "1.x 1.2.3 - 2.0.0"
        shouldNotParse "1.2.3 - 2.0.0 3.x"
  where
    shouldParseTo :: String -> [ComparatorSet] -> Expectation
    shouldParseTo input css = parseRange input `shouldBe` Right (Range css)

    shouldParse :: String -> Expectation
    shouldParse input = isRight (parseRange input) `shouldBe` True

    shouldNotParse :: String -> Expectation
    shouldNotParse input = isRight (parseRange input) `shouldBe` False

    compSet :: Comparator -> ComparatorSet
    compSet = ComparatorSet . pure

    primCompSet :: PrimitiveOperator -> PartialVersion -> ComparatorSet
    primCompSet op = ComparatorSet . pure . PrimitiveComparator op
