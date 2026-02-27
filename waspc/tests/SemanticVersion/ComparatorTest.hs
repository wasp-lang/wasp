module SemanticVersion.ComparatorTest where

import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_Comparator :: Spec
spec_SemanticVersion_Comparator = do
  describe "show" $ do
    it "show primitive operators" $ do
      show (PrimitiveComparator GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (PrimitiveComparator GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (PrimitiveComparator LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (PrimitiveComparator LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (PrimitiveComparator Equal [pv|1.2.3|]) `shouldBe` "1.2.3"
    it "show backwardsCompatibleWith" $ do
      show (BackwardsCompatibleWith [pv|1.2.3|]) `shouldBe` "^1.2.3"
      show (BackwardsCompatibleWith [pv|0.2|]) `shouldBe` "^0.2"
    it "show approximatelyEquvivalentTo" $ do
      show (ApproximatelyEquvivalentTo [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (ApproximatelyEquvivalentTo [pv|1.2|]) `shouldBe` "~1.2"
    it "show x-range" $ do
      show (XRange [pv|1.2|]) `shouldBe` "1.2"
      show (XRange [pv|1|]) `shouldBe` "1"
      show (XRange [pv|*|]) `shouldBe` "*"
    it "show hyphen range" $ do
      show (HyphenRange [pv|1.2.3|] [pv|2.3.4|]) `shouldBe` "1.2.3 - 2.3.4"

  describe "isVersionInRange" $ do
    describe "Primitive operators" $ do
      it ">=1.2 means >=1.2.0" $
        testComparator
          (PrimitiveComparator GreaterThanOrEqual [pv|1.2|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|1.2.5|], True),
            ([v|2.0.0|], True)
          ]
      it ">1.2 means >=1.3.0 (greater than any 1.2.x)" $
        testComparator
          (PrimitiveComparator GreaterThan [pv|1.2|])
          [ ([v|1.2.0|], False),
            ([v|1.2.99|], False),
            ([v|1.3.0|], True),
            ([v|2.0.0|], True)
          ]
      it "<1.2 means <1.2.0" $
        testComparator
          (PrimitiveComparator LessThan [pv|1.2|])
          [ ([v|1.1.9|], True),
            ([v|1.2.0|], False),
            ([v|1.2.5|], False)
          ]
      it "<=1.2 means <1.3.0 (less than or equal to any 1.2.x)" $
        testComparator
          (PrimitiveComparator LessThanOrEqual [pv|1.2|])
          [ ([v|1.1.9|], True),
            ([v|1.2.0|], True),
            ([v|1.2.99|], True),
            ([v|1.3.0|], False)
          ]
      it "=1.2 means >=1.2.0 <1.3.0 (same as X-range)" $
        testComparator
          (PrimitiveComparator Equal [pv|1.2|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|1.2.99|], True),
            ([v|1.3.0|], False)
          ]
      it ">1 means >=2.0.0" $
        testComparator
          (PrimitiveComparator GreaterThan [pv|1|])
          [ ([v|1.0.0|], False),
            ([v|1.99.99|], False),
            ([v|2.0.0|], True)
          ]
      it "<=1 means <2.0.0" $
        testComparator
          (PrimitiveComparator LessThanOrEqual [pv|1|])
          [ ([v|1.0.0|], True),
            ([v|1.99.99|], True),
            ([v|2.0.0|], False)
          ]

    describe "Tilde ranges (~)" $ do
      it "~1.2 allows patch updates (same as ~1.2.0)" $
        testComparator
          (ApproximatelyEquvivalentTo [pv|1.2|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|1.2.5|], True),
            ([v|1.3.0|], False)
          ]
      it "~1 allows minor and patch updates" $
        testComparator
          (ApproximatelyEquvivalentTo [pv|1|])
          [ ([v|0.9.9|], False),
            ([v|1.0.0|], True),
            ([v|1.5.3|], True),
            ([v|1.99.99|], True),
            ([v|2.0.0|], False)
          ]

    describe "Caret ranges (^)" $ do
      it "^1.2 allows minor and patch updates (same as ^1.2.0)" $
        testComparator
          (BackwardsCompatibleWith [pv|1.2|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|1.5.3|], True),
            ([v|1.99.99|], True),
            ([v|2.0.0|], False)
          ]
      it "^1 allows minor and patch updates" $
        testComparator
          (BackwardsCompatibleWith [pv|1|])
          [ ([v|0.9.9|], False),
            ([v|1.0.0|], True),
            ([v|1.5.3|], True),
            ([v|1.99.99|], True),
            ([v|2.0.0|], False)
          ]
      it "^0.2 allows only patch updates" $
        testComparator
          (BackwardsCompatibleWith [pv|0.2|])
          [ ([v|0.1.9|], False),
            ([v|0.2.0|], True),
            ([v|0.2.5|], True),
            ([v|0.3.0|], False)
          ]
      it "^0.0 allows only patch updates" $
        testComparator
          (BackwardsCompatibleWith [pv|0.0|])
          [ ([v|0.0.0|], True),
            ([v|0.0.5|], True),
            ([v|0.1.0|], False)
          ]

    describe "X-Ranges (wildcards)" $ do
      it "* matches any version" $
        testComparator
          (XRange [pv|*|])
          [ ([v|0.0.0|], True),
            ([v|1.2.3|], True),
            ([v|99.99.99|], True)
          ]
      it "1.x matches >=1.0.0 <2.0.0" $
        testComparator
          (XRange [pv|1|])
          [ ([v|0.9.9|], False),
            ([v|1.0.0|], True),
            ([v|1.5.3|], True),
            ([v|1.99.99|], True),
            ([v|2.0.0|], False)
          ]
      it "1.2.x matches >=1.2.0 <1.3.0" $
        testComparator
          (XRange [pv|1.2|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|1.2.99|], True),
            ([v|1.3.0|], False)
          ]
      it "Full version 1.2.3 in X-range matches exactly" $
        testComparator
          (XRange [pv|1.2.3|])
          [ ([v|1.2.2|], False),
            ([v|1.2.3|], True),
            ([v|1.2.4|], False)
          ]

    describe "Hyphen ranges" $ do
      it "1.2 - 2.3.4 matches >=1.2.0 <=2.3.4" $
        testComparator
          (HyphenRange [pv|1.2|] [pv|2.3.4|])
          [ ([v|1.1.9|], False),
            ([v|1.2.0|], True),
            ([v|2.3.4|], True),
            ([v|2.3.5|], False)
          ]
      it "1.2.3 - 2.3 matches >=1.2.3 <2.4.0 (partial upper is exclusive)" $
        testComparator
          (HyphenRange [pv|1.2.3|] [pv|2.3|])
          [ ([v|1.2.2|], False),
            ([v|1.2.3|], True),
            ([v|2.3.99|], True),
            ([v|2.4.0|], False)
          ]
      it "1.2.3 - 2 matches >=1.2.3 <3.0.0" $
        testComparator
          (HyphenRange [pv|1.2.3|] [pv|2|])
          [ ([v|1.2.2|], False),
            ([v|1.2.3|], True),
            ([v|2.99.99|], True),
            ([v|3.0.0|], False)
          ]

  describe "versionBounds" $ do
    let comp ~> expectedInterval =
          it (show comp) $
            versionBounds comp `shouldBe` expectedInterval

    -- Primitive operators
    PrimitiveComparator GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
    PrimitiveComparator GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
    PrimitiveComparator LessThan [pv|1.2|] ~> [vi| (inf, 1.2.0) |]
    PrimitiveComparator LessThanOrEqual [pv|1.2|] ~> [vi| (inf, 1.3.0) |]
    PrimitiveComparator Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]

    -- Tilde range bounds
    ApproximatelyEquvivalentTo [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
    ApproximatelyEquvivalentTo [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    ApproximatelyEquvivalentTo [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]

    -- Caret range bounds
    BackwardsCompatibleWith [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
    BackwardsCompatibleWith [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
    BackwardsCompatibleWith [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]

    -- X-range bounds
    XRange [pv|*|] ~> [vi| [0.0.0, inf) |]
    XRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    XRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    XRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]

    -- Hyphen range bounds
    HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
    HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
  where
    testComparator :: Comparator -> [(Version, Bool)] -> Expectation
    testComparator comp versionsWithResults =
      map (\(ver, _) -> isVersionInInterval (versionBounds comp) ver) versionsWithResults
        `shouldBe` map snd versionsWithResults
