module SemanticVersion.RangeTest where

import Test.Hspec
import Wasp.SemanticVersion

-- TODO(franjo)
spec_SemanticVersion_Range :: Spec
spec_SemanticVersion_Range = do
  describe "show" $ do
    it "show empty range" $ do
      show (mempty :: Range) `shouldBe` ""
    it "show simple range" $ do
      show (Range [lte [v|1.3.6|]]) `shouldBe` "<=1.3.6"
    it "show complex range" $ do
      show
        ( Range [lte [v|1.3.6|] <> backwardsCompatibleWith [v|1.2.0|]]
            <> Range [eq [v|1.2.3|]]
        )
        `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3"

  it "Concatenating version ranges produces union of their comparator sets" $ do
    let v1 = [v|1.0.0|]
    let v2 = [v|2.0.0|]
    let r1 = Range [gt v1, lt v2]
    let r2 = Range [lt v2]
    r1 <> r2 `shouldBe` r1

  describe "isVersionInRange" $ do
    it "No version is in empty range" $
      testRange
        mempty
        [ ([v|0.5.5|], False),
          ([v|1.0.0|], False),
          ([v|1.2.3|], False),
          ([v|1.2.4|], False),
          ([v|1.3.0|], False),
          ([v|2.0.0|], False)
        ]

    describe "Using helper functions (Version-based)" $ do
      it "Recognizes only version v to be in range '=v'" $
        testRange
          (Range [eq [v|1.2.3|]])
          [ ([v|0.5.5|], False),
            ([v|1.0.0|], False),
            ([v|1.2.3|], True),
            ([v|1.2.4|], False),
            ([v|1.3.0|], False),
            ([v|2.0.0|], False)
          ]
      it "Recognizes only versions lesser or equal to v to be in range '<=v'" $
        testRange
          (Range [lte [v|1.2.3|]])
          [ ([v|0.5.5|], True),
            ([v|1.0.0|], True),
            ([v|1.2.3|], True),
            ([v|1.2.4|], False),
            ([v|1.3.0|], False),
            ([v|2.0.0|], False)
          ]
      describe "Recognizes only versions >=v but smaller than next breaking change to be in range '^v'" $ do
        it "when v is of shape x.y.z where x != 0." $
          testRange
            (Range [backwardsCompatibleWith [v|1.2.3|]])
            [ ([v|0.5.5|], False),
              ([v|1.0.0|], False),
              ([v|1.2.3|], True),
              ([v|1.2.4|], True),
              ([v|1.3.0|], True),
              ([v|2.0.0|], False)
            ]
        it "when v is of shape 0.y.z where y != 0." $
          testRange
            (Range [backwardsCompatibleWith [v|0.2.3|]])
            [ ([v|0.0.0|], False),
              ([v|0.1.3|], False),
              ([v|0.2.0|], False),
              ([v|0.2.2|], False),
              ([v|0.2.3|], True),
              ([v|0.2.4|], True),
              ([v|0.3.0|], False),
              ([v|1.0.0|], False)
            ]
        it "when v is of shape 0.0.z." $
          testRange
            (Range [backwardsCompatibleWith [v|0.0.2|]])
            [ ([v|0.0.1|], False),
              ([v|0.0.2|], True),
              ([v|0.0.3|], False),
              ([v|0.1.0|], False),
              ([v|1.0.0|], False)
            ]
        it "Correctly works for complex version range." $
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

      describe "Tilde ranges (~) with full Version" $ do
        it "~1.2.3 allows patch updates only" $
          testRange
            (Range [approximatelyEquvivalentTo [v|1.2.3|]])
            [ ([v|1.2.2|], False),
              ([v|1.2.3|], True),
              ([v|1.2.4|], True),
              ([v|1.2.99|], True),
              ([v|1.3.0|], False),
              ([v|2.0.0|], False)
            ]
        it "~0.2.3 allows patch updates only" $
          testRange
            (Range [approximatelyEquvivalentTo [v|0.2.3|]])
            [ ([v|0.2.2|], False),
              ([v|0.2.3|], True),
              ([v|0.2.4|], True),
              ([v|0.3.0|], False)
            ]

      describe "X-Ranges with full Version" $ do
        it "Full version 1.2.3 matches exactly" $
          testRange
            (Range [xRange [v|1.2.3|]])
            [ ([v|1.2.2|], False),
              ([v|1.2.3|], True),
              ([v|1.2.4|], False)
            ]

      describe "Hyphen ranges with full Version" $ do
        it "1.2.3 - 2.3.4 matches >=1.2.3 <=2.3.4" $
          testRange
            (Range [hyphenRange [v|1.2.3|] [v|2.3.4|]])
            [ ([v|1.2.2|], False),
              ([v|1.2.3|], True),
              ([v|1.5.0|], True),
              ([v|2.3.4|], True),
              ([v|2.3.5|], False)
            ]

  describe "versionBounds" $ do
    describe "Using helper functions (Version-based)" $ do
      let range ~> expectedInterval =
            it (show range) $
              versionBounds range `shouldBe` expectedInterval
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
  where
    testRange :: Range -> [(Version, Bool)] -> Expectation
    testRange range versionsWithResults =
      map (\(ver, _) -> isVersionInRange ver range) versionsWithResults
        `shouldBe` map snd versionsWithResults
