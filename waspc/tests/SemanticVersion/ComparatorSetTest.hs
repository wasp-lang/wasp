module SemanticVersion.ComparatorSetTest where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
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
