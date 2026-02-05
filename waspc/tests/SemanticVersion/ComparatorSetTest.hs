module SemanticVersion.ComparatorSetTest where

import Numeric.Natural
import Test.Hspec
import Wasp.SemanticVersion

-- TODO(franjo)
spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "show" $ do
    it "show approximatelyEquvivalentTo range" $ do
      show (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2.3|]]) `shouldBe` "~1.2.3"
      show (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2|]]) `shouldBe` "~1.2"
    it "show x-range" $ do
      show (Range [ComparatorSet . pure . XRange $ [pv|1.2|]]) `shouldBe` "1.2"
      show (Range [ComparatorSet . pure . XRange $ [pv|1|]]) `shouldBe` "1"
      show (Range [ComparatorSet . pure . XRange $ [pv|*|]]) `shouldBe` "*"
    it "show hyphen range" $ do
      show (Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2.3.4|]]) `shouldBe` "1.2.3 - 2.3.4"

  describe "isVersionInRange with PartialVersion-based comparators" $ do
    describe "Primitive operators with partial versions" $ do
      it ">=1.2 means >=1.2.0" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator GreaterThanOrEqual [pv|1.2|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((1, 2, 5), True),
            ((2, 0, 0), True)
          ]
      it ">1.2 means >=1.3.0 (greater than any 1.2.x)" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator GreaterThan [pv|1.2|]])
          [ ((1, 2, 0), False),
            ((1, 2, 99), False),
            ((1, 3, 0), True),
            ((2, 0, 0), True)
          ]
      it "<1.2 means <1.2.0" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator LessThan [pv|1.2|]])
          [ ((1, 1, 9), True),
            ((1, 2, 0), False),
            ((1, 2, 5), False)
          ]
      it "<=1.2 means <1.3.0 (less than or equal to any 1.2.x)" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator LessThanOrEqual [pv|1.2|]])
          [ ((1, 1, 9), True),
            ((1, 2, 0), True),
            ((1, 2, 99), True),
            ((1, 3, 0), False)
          ]
      it "=1.2 means >=1.2.0 <1.3.0 (same as X-range)" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator Equal [pv|1.2|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((1, 2, 99), True),
            ((1, 3, 0), False)
          ]
      it ">1 means >=2.0.0" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator GreaterThan [pv|1|]])
          [ ((1, 0, 0), False),
            ((1, 99, 99), False),
            ((2, 0, 0), True)
          ]
      it "<=1 means <2.0.0" $
        testRange
          (Range [ComparatorSet . pure $ PrimitiveComparator LessThanOrEqual [pv|1|]])
          [ ((1, 0, 0), True),
            ((1, 99, 99), True),
            ((2, 0, 0), False)
          ]

    describe "Tilde ranges (~) with PartialVersion" $ do
      it "~1.2 allows patch updates (same as ~1.2.0)" $
        testRange
          (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((1, 2, 5), True),
            ((1, 3, 0), False)
          ]
      it "~1 allows minor and patch updates" $
        testRange
          (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1|]])
          [ ((0, 9, 9), False),
            ((1, 0, 0), True),
            ((1, 5, 3), True),
            ((1, 99, 99), True),
            ((2, 0, 0), False)
          ]

    describe "Caret ranges (^) with PartialVersion" $ do
      it "^1.2 allows minor and patch updates (same as ^1.2.0)" $
        testRange
          (Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|1.2|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((1, 5, 3), True),
            ((1, 99, 99), True),
            ((2, 0, 0), False)
          ]
      it "^1 allows minor and patch updates" $
        testRange
          (Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|1|]])
          [ ((0, 9, 9), False),
            ((1, 0, 0), True),
            ((1, 5, 3), True),
            ((1, 99, 99), True),
            ((2, 0, 0), False)
          ]
      it "^0.2 allows only patch updates" $
        testRange
          (Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|0.2|]])
          [ ((0, 1, 9), False),
            ((0, 2, 0), True),
            ((0, 2, 5), True),
            ((0, 3, 0), False)
          ]
      it "^0.0 allows only patch updates" $
        testRange
          (Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|0.0|]])
          [ ((0, 0, 0), True),
            ((0, 0, 5), True),
            ((0, 1, 0), False)
          ]

    describe "X-Ranges (wildcards) with PartialVersion" $ do
      it "* matches any version" $
        testRange
          (Range [ComparatorSet . pure . XRange $ [pv|*|]])
          [ ((0, 0, 0), True),
            ((1, 2, 3), True),
            ((99, 99, 99), True)
          ]
      it "1.x matches >=1.0.0 <2.0.0" $
        testRange
          (Range [ComparatorSet . pure . XRange $ [pv|1|]])
          [ ((0, 9, 9), False),
            ((1, 0, 0), True),
            ((1, 5, 3), True),
            ((1, 99, 99), True),
            ((2, 0, 0), False)
          ]
      it "1.2.x matches >=1.2.0 <1.3.0" $
        testRange
          (Range [ComparatorSet . pure . XRange $ [pv|1.2|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((1, 2, 99), True),
            ((1, 3, 0), False)
          ]
      it "Full version 1.2.3 in X-range matches exactly" $
        testRange
          (Range [ComparatorSet . pure . XRange $ [pv|1.2.3|]])
          [ ((1, 2, 2), False),
            ((1, 2, 3), True),
            ((1, 2, 4), False)
          ]

    describe "Hyphen ranges with PartialVersion" $ do
      it "1.2 - 2.3.4 matches >=1.2.0 <=2.3.4" $
        testRange
          (Range [ComparatorSet . pure $ HyphenRange [pv|1.2|] [pv|2.3.4|]])
          [ ((1, 1, 9), False),
            ((1, 2, 0), True),
            ((2, 3, 4), True),
            ((2, 3, 5), False)
          ]
      it "1.2.3 - 2.3 matches >=1.2.3 <2.4.0 (partial upper is exclusive)" $
        testRange
          (Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2.3|]])
          [ ((1, 2, 2), False),
            ((1, 2, 3), True),
            ((2, 3, 99), True),
            ((2, 4, 0), False)
          ]
      it "1.2.3 - 2 matches >=1.2.3 <3.0.0" $
        testRange
          (Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2|]])
          [ ((1, 2, 2), False),
            ((1, 2, 3), True),
            ((2, 99, 99), True),
            ((3, 0, 0), False)
          ]

  describe "versionBounds with comparators" $ do
    let range ~> expectedInterval =
          it (show range) $
            versionBounds range `shouldBe` expectedInterval

    -- Primitive operators with partial versions
    Range [ComparatorSet . pure $ PrimitiveComparator GreaterThanOrEqual [pv|1.2|]] ~> [vi| [1.2.0, inf) |]
    Range [ComparatorSet . pure $ PrimitiveComparator GreaterThan [pv|1.2|]] ~> [vi| [1.3.0, inf) |]
    Range [ComparatorSet . pure $ PrimitiveComparator LessThan [pv|1.2|]] ~> [vi| (inf, 1.2.0) |]
    Range [ComparatorSet . pure $ PrimitiveComparator LessThanOrEqual [pv|1.2|]] ~> [vi| (inf, 1.3.0) |]
    Range [ComparatorSet . pure $ PrimitiveComparator Equal [pv|1.2|]] ~> [vi| [1.2.0, 1.3.0) |]

    -- Tilde range bounds
    Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2.3|]] ~> [vi| [1.2.3, 1.3.0) |]
    Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2|]] ~> [vi| [1.2.0, 1.3.0) |]
    Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1|]] ~> [vi| [1.0.0, 2.0.0) |]

    -- Caret range bounds
    Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|1.2|]] ~> [vi| [1.2.0, 2.0.0) |]
    Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|0.2|]] ~> [vi| [0.2.0, 0.3.0) |]
    Range [ComparatorSet . pure . BackwardsCompatibleWith $ [pv|0.0|]] ~> [vi| [0.0.0, 0.1.0) |]

    -- X-range bounds
    Range [ComparatorSet . pure . XRange $ [pv|*|]] ~> [vi| [0.0.0, inf) |]
    Range [ComparatorSet . pure . XRange $ [pv|1|]] ~> [vi| [1.0.0, 2.0.0) |]
    Range [ComparatorSet . pure . XRange $ [pv|1.2|]] ~> [vi| [1.2.0, 1.3.0) |]
    Range [ComparatorSet . pure . XRange $ [pv|1.2.3|]] ~> [vi| [1.2.3, 1.2.3] |]

    -- Hyphen range bounds
    Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2.3.4|]] ~> [vi| [1.2.3, 2.3.4] |]
    Range [ComparatorSet . pure $ HyphenRange [pv|1.2|] [pv|2.3.4|]] ~> [vi| [1.2.0, 2.3.4] |]
    Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2.3|]] ~> [vi| [1.2.3, 2.4.0) |]
  where
    testRange :: Range -> [((Natural, Natural, Natural), Bool)] -> Expectation
    testRange range versionsWithResults =
      ( (`isVersionInRange` range) . (\(x, y, z) -> Version x y z)
          <$> map fst versionsWithResults
      )
        `shouldBe` map snd versionsWithResults
