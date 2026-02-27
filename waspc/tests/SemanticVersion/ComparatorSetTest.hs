module SemanticVersion.ComparatorSetTest where

import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "show" $ do
    it "shows single comparator comparator set" $ do
      show (ComparatorSet $ pure $ XRange [pv|1.2|]) `shouldBe` "1.2"
    it "shows multiple comparator comparator set" $ do
      show
        ( ComparatorSet $
            NE.fromList
              [ PrimitiveComparator GreaterThanOrEqual [pv|1.2.0|],
                PrimitiveComparator LessThan [pv|2.0.0|]
              ]
        )
        `shouldBe` ">=1.2.0 <2.0.0"
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
    it ">=1.0.0 <2.0.0 intersects to [1.0.0, 2.0.0)" $
      versionBounds
        ( ComparatorSet $
            NE.fromList
              [ PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|],
                PrimitiveComparator LessThan [pv|2.0.0|]
              ]
        )
        `shouldBe` [vi| [1.0.0, 2.0.0) |]
    it ">1.0.0 <=2.0.0 intersects to (1.0.0, 2.0.0]" $
      versionBounds
        ( ComparatorSet $
            NE.fromList
              [ PrimitiveComparator GreaterThan [pv|1.0.0|],
                PrimitiveComparator LessThanOrEqual [pv|2.0.0|]
              ]
        )
        `shouldBe` [vi| (1.0.0, 2.0.0] |]

  describe "isVersionInRange" $ do
    it ">=1.0.0 <2.0.0 matches versions in range" $
      testComparatorSet
        ( ComparatorSet $
            NE.fromList
              [ PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|],
                PrimitiveComparator LessThan [pv|2.0.0|]
              ]
        )
        [ ([v|0.9.9|], False),
          ([v|1.0.0|], True),
          ([v|1.5.0|], True),
          ([v|1.99.99|], True),
          ([v|2.0.0|], False)
        ]
  where
    testComparatorSet :: ComparatorSet -> [(Version, Bool)] -> Expectation
    testComparatorSet compSet versionsWithResults =
      map (\(ver, _) -> isVersionInInterval (versionBounds compSet) ver) versionsWithResults
        `shouldBe` map snd versionsWithResults
