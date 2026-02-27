module SemanticVersion.RangeTest where

import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_Range :: Spec
spec_SemanticVersion_Range = do
  describe "show" $ do
    it "show empty range" $ do
      show (mempty :: Range)
        `shouldBe` ""
    it "show simple range" $ do
      show (Range [lte [v|1.3.6|]])
        `shouldBe` "<=1.3.6"
    it "show complex range" $ do
      show (Range [lte [v|1.3.6|] <> backwardsCompatibleWith [v|1.2.0|]] <> Range [eq [v|1.2.3|]])
        `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3"

  describe "parsing" $ do
    it "parses minimal possible range" $ do
      parseRange ""
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ XRange Any
                    ]
              ]
          )

    it "parses ranges with single comparator set" $ do
      parseRange ">=1.0.0 <2.0.0"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                      PrimitiveComparator LessThan (Full 2 0 0)
                    ]
              ]
          )
      parseRange "^1.2.3"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $ pure $ BackwardsCompatibleWith (Full 1 2 3)
              ]
          )
    it "parses ranges with multiple comparator sets" $ do
      parseRange ">=1.0.0 <2.0.0 || >=3.0.0 || *"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $
                  NE.fromList
                    [ PrimitiveComparator GreaterThanOrEqual (Full 1 0 0),
                      PrimitiveComparator LessThan (Full 2 0 0)
                    ],
                ComparatorSet $ pure $ PrimitiveComparator GreaterThanOrEqual (Full 3 0 0),
                ComparatorSet $ pure $ XRange Any
              ]
          )
      parseRange "^1.2.3 || ^2.0.0"
        `shouldBe` Right
          ( Range
              [ ComparatorSet $ pure $ BackwardsCompatibleWith (Full 1 2 3),
                ComparatorSet $ pure $ BackwardsCompatibleWith (Full 2 0 0)
              ]
          )

    it "rejects invalid formats" $ do
      isLeft (parseRange "") `shouldBe` True
      isLeft (parseRange "foo") `shouldBe` True

  it "Concatenating version ranges produces union of their comparator sets" $ do
    let v1 = [v|1.0.0|]
    let v2 = [v|2.0.0|]
    let r1 = Range [gt v1, lt v2]
    let r2 = Range [lt v2]
    r1 <> r2 `shouldBe` r1

  describe "isVersionInRange" $ do
    let testRange range versionsWithResults =
          map (\(ver, _) -> isVersionInRange ver range) versionsWithResults
            `shouldBe` map snd versionsWithResults

    it "No version is in empty range" $
      testRange
        mempty
        [ ([v|0.5.5|], False),
          ([v|1.0.0|], False),
          ([v|1.2.3|], False),
          ([v|2.0.0|], False)
        ]
    it "Complex range" $
      testRange
        (Range [lte [v|1.2.3|] <> backwardsCompatibleWith [v|1.1.0|], eq [v|0.5.6|]])
        [ ([v|0.5.5|], False),
          ([v|0.5.6|], True),
          ([v|0.5.7|], False),
          ([v|1.0.9|], False),
          ([v|1.1.0|], True),
          ([v|1.1.9|], True),
          ([v|1.2.3|], True),
          ([v|1.2.4|], False),
          ([v|1.3.0|], False),
          ([v|2.0.0|], False)
        ]

  describe "versionBounds" $ do
    let range ~> expectedInterval =
          it (show range) $ versionBounds range `shouldBe` expectedInterval

    Range [] ~> [vi| (inf, inf) |]
    Range [gt [v|0.1.2|]] ~> [vi| (0.1.2, inf) |]
    Range [gt [v|0.1.2|] <> lt [v|0.2.0|]] ~> [vi| (0.1.2, 0.2.0) |]
    Range [lte [v|1.2.3|]] ~> [vi| (inf, 1.2.3] |]
    Range [backwardsCompatibleWith [v|0.2.3|]] ~> [vi| [0.2.3, 0.3.0) |]
    Range [backwardsCompatibleWith [v|1.2.3|]] ~> [vi| [1.2.3, 2.0.0) |]
    Range [lte [v|1.2.3|] <> backwardsCompatibleWith [v|1.1.0|], eq [v|0.5.6|]] ~> [vi| [0.5.6, 1.2.3] |]

  describe "doesVersionRangeAllowMajorChanges" $ do
    let range ~> expected =
          it (show range) $
            doesVersionRangeAllowMajorChanges range `shouldBe` expected
    Range [] ~> True
    Range [gt [v|1.1.2|]] ~> True
    Range [gt [v|0.1.2|] <> lt [v|0.2.0|]] ~> False
    Range [gt [v|0.1.2|] <> lte [v|0.2.0|]] ~> True
    Range [gt [v|2.0.0|] <> lte [v|3.0.0|]] ~> True
    Range [gt [v|2.0.0|] <> lt [v|3.0.0|]] ~> False
    Range [lte [v|2.9.99|]] ~> True
    Range [backwardsCompatibleWith [v|0.2.3|]] ~> False
    Range [lte [v|1.2.3|] <> backwardsCompatibleWith [v|1.1.0|], eq [v|0.5.6|]] ~> True
