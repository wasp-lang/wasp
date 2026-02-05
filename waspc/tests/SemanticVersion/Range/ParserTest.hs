{-# LANGUAGE QuasiQuotes #-}

module SemanticVersion.Range.ParserTest where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_Range_Parser :: Spec
spec_SemanticVersion_Range_Parser = do
  describe "parseRange" $ do
    it "parses primitive operators with full versions" $ do
      parseRange ">=1.2.3" `shouldBe` Right (Range [gte [v|1.2.3|]])
      parseRange "<=1.2.3" `shouldBe` Right (Range [lte [v|1.2.3|]])
      parseRange ">1.2.3" `shouldBe` Right (Range [gt [v|1.2.3|]])
      parseRange "<1.2.3" `shouldBe` Right (Range [lt [v|1.2.3|]])
      parseRange "=1.2.3" `shouldBe` Right (Range [eq [v|1.2.3|]])

    it "parses primitive operators with partial versions" $ do
      parseRange ">=1.2" `shouldBe` Right (Range [primCompSet GreaterThanOrEqual [pv|1.2|]])
      parseRange "<=1.2" `shouldBe` Right (Range [primCompSet LessThanOrEqual [pv|1.2|]])
      parseRange ">1" `shouldBe` Right (Range [primCompSet GreaterThan [pv|1|]])
      parseRange "<1" `shouldBe` Right (Range [primCompSet LessThan [pv|1|]])
      parseRange "=1.2" `shouldBe` Right (Range [primCompSet Equal [pv|1.2|]])

    it "parses caret ranges" $ do
      parseRange "^1.2.3" `shouldBe` Right (Range [compSet $ BackwardsCompatibleWith [pv|1.2.3|]])
      parseRange "^1.2" `shouldBe` Right (Range [compSet $ BackwardsCompatibleWith [pv|1.2|]])
      parseRange "^1" `shouldBe` Right (Range [compSet $ BackwardsCompatibleWith [pv|1|]])

    it "parses approximatelyEquvivalentTo ranges" $ do
      parseRange "~1.2.3" `shouldBe` Right (Range [compSet $ ApproximatelyEquvivalentTo [pv|1.2.3|]])
      parseRange "~1.2" `shouldBe` Right (Range [compSet $ ApproximatelyEquvivalentTo [pv|1.2|]])
      parseRange "~1" `shouldBe` Right (Range [compSet $ ApproximatelyEquvivalentTo [pv|1|]])

    it "parses X-ranges" $ do
      parseRange "*" `shouldBe` Right (Range [compSet $ XRange Any])
      parseRange "x" `shouldBe` Right (Range [compSet $ XRange Any])
      parseRange "X" `shouldBe` Right (Range [compSet $ XRange Any])
      parseRange "1" `shouldBe` Right (Range [compSet $ XRange [pv|1|]])
      parseRange "1.x" `shouldBe` Right (Range [compSet $ XRange [pv|1|]])
      parseRange "1.x.x" `shouldBe` Right (Range [compSet $ XRange [pv|1|]])
      parseRange "1.2" `shouldBe` Right (Range [compSet $ XRange [pv|1.2|]])
      parseRange "1.2.x" `shouldBe` Right (Range [compSet $ XRange [pv|1.2|]])
      parseRange "1.2.3" `shouldBe` Right (Range [compSet $ XRange [pv|1.2.3|]])

    it "parses hyphen ranges" $ do
      parseRange "1.2.3 - 2.3.4" `shouldBe` Right (Range [compSet $ HyphenRange [pv|1.2.3|] [pv|2.3.4|]])
      parseRange "1.2 - 2.3.4" `shouldBe` Right (Range [compSet $ HyphenRange [pv|1.2|] [pv|2.3.4|]])
      parseRange "1.2.3 - 2.3" `shouldBe` Right (Range [compSet $ HyphenRange [pv|1.2.3|] [pv|2.3|]])
      parseRange "1 - 3" `shouldBe` Right (Range [compSet $ HyphenRange [pv|1|] [pv|3|]])

    it "parses comparator sets with multiple comparators (AND)" $ do
      parseRange ">=1.0.0 <2.0.0"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|],
                      PrimitiveComparator LessThan [pv|2.0.0|]
                    ]
              ]
          )
      parseRange ">1.0.0 <=2.0.0"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ PrimitiveComparator GreaterThan [pv|1.0.0|],
                      PrimitiveComparator LessThanOrEqual [pv|2.0.0|]
                    ]
              ]
          )

    it "parses ranges with multiple comparator sets (OR)" $ do
      parseRange ">=1.0.0 <2.0.0 || >=3.0.0"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|],
                      PrimitiveComparator LessThan [pv|2.0.0|]
                    ],
                primCompSet GreaterThanOrEqual [pv|3.0.0|]
              ]
          )
      parseRange "^1.2.3 || ^2.0.0"
        `shouldBe` Right
          ( Range
              [ compSet $ BackwardsCompatibleWith [pv|1.2.3|],
                compSet $ BackwardsCompatibleWith [pv|2.0.0|]
              ]
          )

    -- Hyphen ranges are mutually exclusive with other comparators within the same comparator set.
    -- They can only appear alone in a comparator set, or combined with other sets via ||.
    describe "hyphen ranges cannot be combined with other comparators in the same comparator set" $ do
      it "allows hyphen range alone" $ do
        isRight (parseRange "1.2.3 - 2.0.0") `shouldBe` True

      it "allows hyphen range with || (different comparator sets)" $ do
        isRight (parseRange "1.2.3 - 2.0.0 || >=3.0.0") `shouldBe` True
        isRight (parseRange ">=0.5.0 || 1.2.3 - 2.0.0") `shouldBe` True
        isRight (parseRange "^1.0.0 || ~1.0.0 || 1.2.3 - 2.0.0") `shouldBe` True
        isRight (parseRange "1.0.0 - 2.0.0 || 3.0.0 - 4.0.0") `shouldBe` True

      it "rejects primitive comparator with hyphen range in the same comparator set" $ do
        isRight (parseRange ">=1.2.3 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange ">1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "<2.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 >=3.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 <3.0.0") `shouldBe` False

      it "rejects caret/tilde with hyphen range in the same comparator set" $ do
        isRight (parseRange "^1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "~1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 ^3.0.0") `shouldBe` False

      it "rejects x-range with hyphen range in the same comparator set" $ do
        isRight (parseRange "1.x 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 3.x") `shouldBe` False
  where
    compSet :: Comparator -> ComparatorSet
    compSet = ComparatorSet . pure

    primCompSet :: PrimitiveOperator -> PartialVersion -> ComparatorSet
    primCompSet op = ComparatorSet . pure . PrimitiveComparator op
