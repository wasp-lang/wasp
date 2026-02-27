module SemanticVersion.ComparatorSetTest where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion

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
    let strictParseCompSet = P.parse (comparatorSetParser *> P.eof) ""

    it "parses comparator sets with multiple comparators (AND)" $ do
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
                  ApproximatelyEquvivalentTo (MajorMinor 0 1)
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
                  ApproximatelyEquvivalentTo (MajorMinor 0 1)
                ]
          )

    describe "hyphen ranges cannot be combined with other comparators" $ do
      let shouldParseCompSet input = isRight (strictParseCompSet input) `shouldBe` True
      let shouldNotParseCompSet input = isRight (strictParseCompSet input) `shouldBe` False

      it "parses hyphen ranges as sole comparator in a set" $ do
        shouldParseCompSet "1.2.3 - 2.0.0"
        shouldParseCompSet "1 - 3"

      it "rejects comparator sets that mix a hyphen range with other comparators" $ do
        shouldNotParseCompSet ">=1.2.3 1.2.3 - 2.0.0"
        shouldNotParseCompSet ">1.0.0 1.2.3 - 2.0.0"
        shouldNotParseCompSet "<2.0.0 1.2.3 - 2.0.0"
        shouldNotParseCompSet "1.2.3 - 2.0.0 >=3.0.0"
        shouldNotParseCompSet "1.2.3 - 2.0.0 <3.0.0"
        shouldNotParseCompSet "^1.0.0 1.2.3 - 2.0.0"
        shouldNotParseCompSet "~1.0.0 1.2.3 - 2.0.0"
        shouldNotParseCompSet "1.2.3 - 2.0.0 ^3.0.0"
        shouldNotParseCompSet "1.x 1.2.3 - 2.0.0"
        shouldNotParseCompSet "1.2.3 - 2.0.0 3.x"

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
    [BackwardsCompatibleWith [pv|1.0.0|], ApproximatelyEquvivalentTo [pv|1.2.0|]]
      ~> [vi| [1.2.0, 1.3.0) |]

    -- Three comparators: the tightest wins on each side
    [PrimitiveComparator GreaterThan [pv|0.5.0|], PrimitiveComparator LessThan [pv|3.0.0|], BackwardsCompatibleWith [pv|1.0.0|]]
      ~> [vi| [1.0.0, 2.0.0) |]

    -- Exclusive beats inclusive at the same version
    [PrimitiveComparator GreaterThan [pv|1.0.0|], PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|]]
      ~> [vi| (1.0.0, inf) |]
    [PrimitiveComparator LessThan [pv|2.0.0|], PrimitiveComparator LessThanOrEqual [pv|2.0.0|]]
      ~> [vi| (inf, 2.0.0) |]
