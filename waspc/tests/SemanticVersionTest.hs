module SemanticVersionTest where

import Data.Either (isRight)
import Numeric.Natural
import Test.Hspec
import Wasp.SemanticVersion
import Wasp.SemanticVersion.PartialVersion (pv)
import Wasp.SemanticVersion.Version (v)
import Wasp.SemanticVersion.VersionBound (HasVersionBounds (versionBounds), vi)

spec_SemanticVersion :: Spec
spec_SemanticVersion = do
  describe "`show` produces valid semver strings" $ do
    it "show empty Range" $ do
      show (mempty :: Range) `shouldBe` ""
    it "show complex Range (using helper functions with Version)" $ do
      show
        ( Range [lte (Version 1 3 6) <> backwardsCompatibleWith (Version 1 2 0)]
            <> Range [eq (Version 1 2 3)]
        )
        `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3"
    it "show approximatelyEquvivalentTo range (using constructor with PartialVersion)" $ do
      show (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2.3|]]) `shouldBe` "~1.2.3"
      show (Range [ComparatorSet . pure . ApproximatelyEquvivalentTo $ [pv|1.2|]]) `shouldBe` "~1.2"
    it "show x-range (using constructor with PartialVersion)" $ do
      show (Range [ComparatorSet . pure . XRange $ [pv|1.2|]]) `shouldBe` "1.2"
      show (Range [ComparatorSet . pure . XRange $ [pv|1|]]) `shouldBe` "1"
      show (Range [ComparatorSet . pure . XRange $ [pv|*|]]) `shouldBe` "*"
    it "show hyphen range (using constructor with PartialVersion)" $ do
      show (Range [ComparatorSet . pure $ HyphenRange [pv|1.2.3|] [pv|2.3.4|]]) `shouldBe` "1.2.3 - 2.3.4"

  it "Concatenating version ranges produces union of their comparator sets" $ do
    let v1 = Version 1 0 0
    let v2 = Version 2 0 0
    let r1 = Range [gt v1, lt v2]
    let r2 = Range [lt v2]
    r1 <> r2 `shouldBe` r1

  describe "isVersionInRange" $ do
    it "No version is in empty range" $
      testRange
        mempty
        [ ((0, 5, 5), False),
          ((1, 0, 0), False),
          ((1, 2, 3), False),
          ((1, 2, 4), False),
          ((1, 3, 0), False),
          ((2, 0, 0), False)
        ]

    describe "Using helper functions (Version-based)" $ do
      it "Recognizes only version v to be in range '=v'" $
        testRange
          (Range [eq (Version 1 2 3)])
          [ ((0, 5, 5), False),
            ((1, 0, 0), False),
            ((1, 2, 3), True),
            ((1, 2, 4), False),
            ((1, 3, 0), False),
            ((2, 0, 0), False)
          ]
      it "Recognizes only versions lesser or equal to v to be in range '<=v'" $
        testRange
          (Range [lte (Version 1 2 3)])
          [ ((0, 5, 5), True),
            ((1, 0, 0), True),
            ((1, 2, 3), True),
            ((1, 2, 4), False),
            ((1, 3, 0), False),
            ((2, 0, 0), False)
          ]
      describe "Recognizes only versions >=v but smaller than next breaking change to be in range '^v'" $ do
        it "when v is of shape x.y.z where x != 0." $
          testRange
            (Range [backwardsCompatibleWith (Version 1 2 3)])
            [ ((0, 5, 5), False),
              ((1, 0, 0), False),
              ((1, 2, 3), True),
              ((1, 2, 4), True),
              ((1, 3, 0), True),
              ((2, 0, 0), False)
            ]
        it "when v is of shape 0.y.z where y != 0." $
          testRange
            (Range [backwardsCompatibleWith (Version 0 2 3)])
            [ ((0, 0, 0), False),
              ((0, 1, 3), False),
              ((0, 2, 0), False),
              ((0, 2, 2), False),
              ((0, 2, 3), True),
              ((0, 2, 4), True),
              ((0, 3, 0), False),
              ((1, 0, 0), False)
            ]
        it "when v is of shape 0.0.z." $
          testRange
            (Range [backwardsCompatibleWith (Version 0 0 2)])
            [ ((0, 0, 1), False),
              ((0, 0, 2), True),
              ((0, 0, 3), False),
              ((0, 1, 0), False),
              ((1, 0, 0), False)
            ]
        it "Correctly works for complex version range." $
          testRange
            (Range [lte (Version 1 2 3) <> backwardsCompatibleWith (Version 1 1 0), eq (Version 0 5 6)])
            [ ((0, 5, 5), False),
              ((0, 5, 6), True),
              ((0, 5, 7), False),
              ((1, 0, 9), False),
              ((1, 1, 0), True),
              ((1, 1, 9), True),
              ((1, 2, 3), True),
              ((1, 2, 4), False),
              ((1, 3, 0), False),
              ((2, 0, 0), False)
            ]

      describe "Tilde ranges (~) with full Version" $ do
        it "~1.2.3 allows patch updates only" $
          testRange
            (Range [approximatelyEquvivalentTo [v|1.2.3|]])
            [ ((1, 2, 2), False),
              ((1, 2, 3), True),
              ((1, 2, 4), True),
              ((1, 2, 99), True),
              ((1, 3, 0), False),
              ((2, 0, 0), False)
            ]
        it "~0.2.3 allows patch updates only" $
          testRange
            (Range [approximatelyEquvivalentTo [v|0.2.3|]])
            [ ((0, 2, 2), False),
              ((0, 2, 3), True),
              ((0, 2, 4), True),
              ((0, 3, 0), False)
            ]

      describe "X-Ranges with full Version" $ do
        it "Full version 1.2.3 matches exactly" $
          testRange
            (Range [xRange [v|1.2.3|]])
            [ ((1, 2, 2), False),
              ((1, 2, 3), True),
              ((1, 2, 4), False)
            ]

      describe "Hyphen ranges with full Version" $ do
        it "1.2.3 - 2.3.4 matches >=1.2.3 <=2.3.4" $
          testRange
            (Range [hyphenRange [v|1.2.3|] [v|2.3.4|]])
            [ ((1, 2, 2), False),
              ((1, 2, 3), True),
              ((1, 5, 0), True),
              ((2, 3, 4), True),
              ((2, 3, 5), False)
            ]

    describe "Using constructor directly (PartialVersion-based)" $ do
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

    describe "Using constructor directly (PartialVersion-based)" $ do
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

  describe "parseRange" $ do
    it "parses primitive operators with full versions" $ do
      parseRange ">=1.2.3" `shouldBe` Right (Range [gte [v|1.2.3|]])
      parseRange "<=1.2.3" `shouldBe` Right (Range [lte [v|1.2.3|]])
      parseRange ">1.2.3" `shouldBe` Right (Range [gt [v|1.2.3|]])
      parseRange "<1.2.3" `shouldBe` Right (Range [lt [v|1.2.3|]])
      parseRange "=1.2.3" `shouldBe` Right (Range [eq [v|1.2.3|]])

    it "parses primitive operators with partial versions" $ do
      isRight (parseRange ">=1.2") `shouldBe` True
      isRight (parseRange "<=1.2") `shouldBe` True
      isRight (parseRange ">1") `shouldBe` True
      isRight (parseRange "<1") `shouldBe` True
      isRight (parseRange "=1.2") `shouldBe` True

    it "parses caret ranges" $ do
      isRight (parseRange "^1.2.3") `shouldBe` True
      isRight (parseRange "^0.2.3") `shouldBe` True
      isRight (parseRange "^1.2") `shouldBe` True
      isRight (parseRange "^1") `shouldBe` True

    it "parses approximatelyEquvivalentTo ranges" $ do
      isRight (parseRange "~1.2.3") `shouldBe` True
      isRight (parseRange "~1.2") `shouldBe` True
      isRight (parseRange "~1") `shouldBe` True

    it "parses X-ranges" $ do
      isRight (parseRange "*") `shouldBe` True
      isRight (parseRange "1") `shouldBe` True
      isRight (parseRange "1.2") `shouldBe` True
      isRight (parseRange "1.2.3") `shouldBe` True
      isRight (parseRange "1.x") `shouldBe` True
      isRight (parseRange "1.2.x") `shouldBe` True

    it "parses hyphen ranges" $ do
      isRight (parseRange "1.2.3 - 2.3.4") `shouldBe` True
      isRight (parseRange "1.2 - 2.3.4") `shouldBe` True
      isRight (parseRange "1.2.3 - 2.3") `shouldBe` True

    it "parses comparator sets (AND)" $ do
      isRight (parseRange ">=1.0.0 <2.0.0") `shouldBe` True
      isRight (parseRange ">1.0.0 <=2.0.0") `shouldBe` True

    it "parses ranges with OR (||)" $ do
      isRight (parseRange ">=1.0.0 <2.0.0 || >=3.0.0") `shouldBe` True
      isRight (parseRange "^1.2.3 || ^2.0.0") `shouldBe` True

    -- Hyphen ranges are mutually exclusive with other comparators within the same comparator set.
    -- They can only appear alone in a comparator set, or combined with other sets via ||.
    describe "hyphen ranges cannot be combined with other comparators in same set" $ do
      it "rejects primitive comparator followed by hyphen range" $ do
        isRight (parseRange ">=1.2.3 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange ">1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "<2.0.0 1.2.3 - 2.0.0") `shouldBe` False

      it "rejects hyphen range followed by primitive comparator" $ do
        isRight (parseRange "1.2.3 - 2.0.0 >=3.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 <3.0.0") `shouldBe` False

      it "rejects caret/tilde with hyphen range in same set" $ do
        isRight (parseRange "^1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "~1.0.0 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 ^3.0.0") `shouldBe` False

      it "rejects x-range with hyphen range in same set" $ do
        isRight (parseRange "1.x 1.2.3 - 2.0.0") `shouldBe` False
        isRight (parseRange "1.2.3 - 2.0.0 3.x") `shouldBe` False

      it "allows hyphen range alone" $ do
        isRight (parseRange "1.2.3 - 2.0.0") `shouldBe` True

      it "allows hyphen range with || (different comparator sets)" $ do
        isRight (parseRange "1.2.3 - 2.0.0 || >=3.0.0") `shouldBe` True
        isRight (parseRange ">=0.5.0 || 1.2.3 - 2.0.0") `shouldBe` True
        isRight (parseRange "1.0.0 - 2.0.0 || 3.0.0 - 4.0.0") `shouldBe` True

    it "parsed ranges match correctly" $ do
      let range1 = unsafeParseRange ">=1.0.0 <2.0.0"
      isVersionInRange (Version 1 5 0) range1 `shouldBe` True
      isVersionInRange (Version 2 0 0) range1 `shouldBe` False

      let range2 = unsafeParseRange "^1.2.3 || ^2.0.0"
      isVersionInRange (Version 1 5 0) range2 `shouldBe` True
      isVersionInRange (Version 2 5 0) range2 `shouldBe` True
      isVersionInRange (Version 3 0 0) range2 `shouldBe` False

    it "parsed ranges with partial versions match correctly" $ do
      let range1 = unsafeParseRange ">=1.2 <2"
      isVersionInRange (Version 1 2 0) range1 `shouldBe` True
      isVersionInRange (Version 1 99 99) range1 `shouldBe` True
      isVersionInRange (Version 2 0 0) range1 `shouldBe` False

      let range2 = unsafeParseRange ">1 <=2"
      isVersionInRange (Version 1 99 99) range2 `shouldBe` False
      isVersionInRange (Version 2 0 0) range2 `shouldBe` True
      isVersionInRange (Version 2 99 99) range2 `shouldBe` True
      isVersionInRange (Version 3 0 0) range2 `shouldBe` False
  where
    testRange :: Range -> [((Natural, Natural, Natural), Bool)] -> Expectation
    testRange range versionsWithResults =
      ( (`isVersionInRange` range) . (\(x, y, z) -> Version x y z)
          <$> map fst versionsWithResults
      )
        `shouldBe` map snd versionsWithResults

    unsafeParseRange :: String -> Range
    unsafeParseRange = either (error . show) id . parseRange
