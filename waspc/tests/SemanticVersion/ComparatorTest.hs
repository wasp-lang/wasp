module SemanticVersion.ComparatorTest where

import Test.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion_Comparator :: Spec
spec_SemanticVersion_Comparator = do
  describe "show" $ do
    it "primitive operators" $ do
      show (PrimitiveComparator GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (PrimitiveComparator GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (PrimitiveComparator LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (PrimitiveComparator LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (PrimitiveComparator Equal [pv|1.2.3|]) `shouldBe` "1.2.3"
    it "backwardsCompatibleWith" $ do
      show (BackwardsCompatibleWith [pv|1.2.3|]) `shouldBe` "^1.2.3"
      show (BackwardsCompatibleWith [pv|0.2|]) `shouldBe` "^0.2"
    it "approximatelyEquvivalentTo" $ do
      show (ApproximatelyEquvivalentTo [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (ApproximatelyEquvivalentTo [pv|1.2|]) `shouldBe` "~1.2"
    it "x-range" $ do
      show (XRange [pv|1.2|]) `shouldBe` "1.2"
      show (XRange [pv|1|]) `shouldBe` "1"
      show (XRange [pv|*|]) `shouldBe` "*"
    it "hyphen range" $ do
      show (HyphenRange [pv|1.2.3|] [pv|2.3.4|]) `shouldBe` "1.2.3 - 2.3.4"

  describe "versionBounds" $ do
    let comp ~> expectedInterval =
          it (show comp) $ versionBounds comp `shouldBe` expectedInterval

    -- Primitive: GreaterThanOrEqual
    PrimitiveComparator GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
    PrimitiveComparator GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
    PrimitiveComparator GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]

    -- Primitive: GreaterThan
    PrimitiveComparator GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
    PrimitiveComparator GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
    PrimitiveComparator GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]

    -- Primitive: LessThan
    PrimitiveComparator LessThan [pv|1.2.3|] ~> [vi| (inf, 1.2.3) |]
    PrimitiveComparator LessThan [pv|1.2|] ~> [vi| (inf, 1.2.0) |]
    PrimitiveComparator LessThan [pv|1|] ~> [vi| (inf, 1.0.0) |]

    -- Primitive: LessThanOrEqual
    PrimitiveComparator LessThanOrEqual [pv|1.2.3|] ~> [vi| (inf, 1.2.3] |]
    PrimitiveComparator LessThanOrEqual [pv|1.2|] ~> [vi| (inf, 1.3.0) |]
    PrimitiveComparator LessThanOrEqual [pv|1|] ~> [vi| (inf, 2.0.0) |]

    -- Primitive: Equal
    PrimitiveComparator Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
    PrimitiveComparator Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    PrimitiveComparator Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]

    -- Tilde range bounds
    ApproximatelyEquvivalentTo [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
    ApproximatelyEquvivalentTo [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    ApproximatelyEquvivalentTo [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]

    -- Caret range bounds
    BackwardsCompatibleWith [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
    BackwardsCompatibleWith [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
    BackwardsCompatibleWith [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
    BackwardsCompatibleWith [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
    BackwardsCompatibleWith [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
    BackwardsCompatibleWith [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
    BackwardsCompatibleWith [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]

    -- X-range bounds
    XRange [pv|*|] ~> [vi| [0.0.0, inf) |]
    XRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    XRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    XRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]

    -- Hyphen range bounds
    HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
    HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
    HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
