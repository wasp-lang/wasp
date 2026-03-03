module SemanticVersion.ComparatorSetTest where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
import Wasp.SemanticVersion.ComparatorSet
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "show" $ do
    it "shows single comparator comparator set" $ do
      show
        ( ComparatorSet $
            NE.fromList
              [ XRange [pv|1.2|]
              ]
        )
        `shouldBe` "1.2"
    it "shows multiple comparator comparator set" $ do
      show
        ( ComparatorSet $
            NE.fromList
              [ PrimitiveComparator GreaterThanOrEqual [pv|1.2.0|],
                PrimitiveComparator LessThan [pv|2.0.0|],
                BackwardsCompatibleWith [pv|2.1.3|]
              ]
        )
        `shouldBe` ">=1.2.0 <2.0.0 ^2.1.3"

  describe "comparatorSetParser" $ do
    let parseCompSet = P.parse comparatorSetParser ""

    it "parses empty input correctly" $ do
      parseCompSet ""
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ XRange Any
                ]
          )

    it "parses comparator sets with single comparator" $ do
      parseCompSet ">=1.0.0"
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0)
                ]
          )

    it "parses comparator sets with multiple comparators" $ do
      parseCompSet ">=1.0.0 <2.0.0"
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                  PrimitiveComparator LessThan (Full 2 0 0)
                ]
          )
      parseCompSet ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X"
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ PrimitiveComparator GreaterThan (Full 1 0 0),
                  PrimitiveComparator LessThanOrEqual (Full 2 0 0),
                  BackwardsCompatibleWith (MajorMinor 1 2),
                  XRange Any,
                  ApproximatelyEquivalentTo (MajorMinor 0 1)
                ]
          )

    it "parses comparator sets with trailing content" $ do
      parseCompSet ">=1.0.0 <2.0.0 || 1"
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                  PrimitiveComparator LessThan (Full 2 0 0)
                ]
          )
      parseCompSet ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
        `shouldBe` Right
          ( ComparatorSet $
              NE.fromList
                [ PrimitiveComparator GreaterThan (Full 1 0 0),
                  PrimitiveComparator LessThanOrEqual (Full 2 0 0),
                  BackwardsCompatibleWith (MajorMinor 1 2),
                  XRange Any,
                  ApproximatelyEquivalentTo (MajorMinor 0 1)
                ]
          )

    describe "hyphen ranges cannot be combined with other comparators" $ do
      -- We must use a strict parser here because "1.2.3 - 2.0.0 3.x" will be parsed
      -- as "1.2.3 - 2.0.0" hyphen range comparator set with " 3.x" suffix.
      let strictParseCompSet = P.parse (comparatorSetParser *> P.eof) ""

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

  describe "versionBounds" $ do
    let comps ~> expectedInterval =
          let compSet = ComparatorSet (NE.fromList comps)
           in it (show compSet) $ versionBounds compSet `shouldBe` expectedInterval

    -- Basic inclusive/exclusive combinations
    [PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|], PrimitiveComparator LessThan [pv|2.0.0|]]
      ~> [vi| [1.0.0, 2.0.0) |]
    [PrimitiveComparator GreaterThan [pv|1.0.0|], PrimitiveComparator LessThanOrEqual [pv|2.0.0|]]
      ~> [vi| (1.0.0, 2.0.0] |]

    -- Caret narrows an open upper bound
    [PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|], BackwardsCompatibleWith [pv|1.2.0|]]
      ~> [vi| [1.2.0, 2.0.0) |]

    -- Tilde narrows further than caret
    [BackwardsCompatibleWith [pv|1.0.0|], ApproximatelyEquivalentTo [pv|1.2.0|]]
      ~> [vi| [1.2.0, 1.3.0) |]

    -- Three comparators: the tightest wins on each side
    [PrimitiveComparator GreaterThan [pv|0.5.0|], PrimitiveComparator LessThan [pv|3.0.0|], BackwardsCompatibleWith [pv|1.0.0|]]
      ~> [vi| [1.0.0, 2.0.0) |]

    -- Exclusive beats inclusive at the same version
    [PrimitiveComparator GreaterThan [pv|1.0.0|], PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|]]
      ~> [vi| (1.0.0, inf) |]
    [PrimitiveComparator LessThan [pv|2.0.0|], PrimitiveComparator LessThanOrEqual [pv|2.0.0|]]
      ~> [vi| [0.0.0, 2.0.0) |]
