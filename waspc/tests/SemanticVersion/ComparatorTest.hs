module SemanticVersion.ComparatorTest where

import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.VersionBound

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
      show (ApproximatelyEquivalentTo [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (ApproximatelyEquivalentTo [pv|1.2|]) `shouldBe` "~1.2"
    it "x-range" $ do
      show (XRange [pv|1.2|]) `shouldBe` "1.2"
      show (XRange [pv|1|]) `shouldBe` "1"
      show (XRange [pv|*|]) `shouldBe` "*"
    it "hyphen range" $ do
      show (HyphenRange [pv|1.2.3|] [pv|2.3.4|]) `shouldBe` "1.2.3 - 2.3.4"

  describe "simpleComparatorParser" $ do
    let parseComp = P.parse simpleComparatorParser ""
        strictParseComp = P.parse (simpleComparatorParser *> P.eof) ""

    it "parses empty input correctly" $ do
      parseComp "" `shouldBe` Right (XRange Any)

    it "parses primitive operator comparators" $ do
      parseComp ">=1.2.3" `shouldBe` Right (PrimitiveComparator GreaterThanOrEqual (Full 1 2 3))
      parseComp "<=1.2.3" `shouldBe` Right (PrimitiveComparator LessThanOrEqual (Full 1 2 3))
      parseComp ">1.2.3" `shouldBe` Right (PrimitiveComparator GreaterThan (Full 1 2 3))
      parseComp "<1.2.3" `shouldBe` Right (PrimitiveComparator LessThan (Full 1 2 3))
      parseComp "=1.2.3" `shouldBe` Right (PrimitiveComparator Equal (Full 1 2 3))
      parseComp ">=1.2" `shouldBe` Right (PrimitiveComparator GreaterThanOrEqual (MajorMinor 1 2))
      parseComp "<=1.2" `shouldBe` Right (PrimitiveComparator LessThanOrEqual (MajorMinor 1 2))
      parseComp "=1.2" `shouldBe` Right (PrimitiveComparator Equal (MajorMinor 1 2))
      parseComp ">1" `shouldBe` Right (PrimitiveComparator GreaterThan (Major 1))
      parseComp "<1" `shouldBe` Right (PrimitiveComparator LessThan (Major 1))

    it "parses caret comparators" $ do
      parseComp "^1.2.3" `shouldBe` Right (BackwardsCompatibleWith (Full 1 2 3))
      parseComp "^1.2" `shouldBe` Right (BackwardsCompatibleWith (MajorMinor 1 2))
      parseComp "^1" `shouldBe` Right (BackwardsCompatibleWith (Major 1))

    it "parses tilde comparators" $ do
      parseComp "~1.2.3" `shouldBe` Right (ApproximatelyEquivalentTo (Full 1 2 3))
      parseComp "~1.2" `shouldBe` Right (ApproximatelyEquivalentTo (MajorMinor 1 2))
      parseComp "~1" `shouldBe` Right (ApproximatelyEquivalentTo (Major 1))

    it "parses X-range comparators" $ do
      parseComp "*" `shouldBe` Right (XRange Any)
      parseComp "x" `shouldBe` Right (XRange Any)
      parseComp "X" `shouldBe` Right (XRange Any)
      parseComp "1" `shouldBe` Right (XRange (Major 1))
      parseComp "1.x" `shouldBe` Right (XRange (Major 1))
      parseComp "1.x.x" `shouldBe` Right (XRange (Major 1))
      parseComp "1.2" `shouldBe` Right (XRange (MajorMinor 1 2))
      parseComp "1.2.x" `shouldBe` Right (XRange (MajorMinor 1 2))
      parseComp "1.2.3" `shouldBe` Right (XRange (Full 1 2 3))

    it "parses simple comparators with trailing content" $ do
      parseComp "* 1.2.3" `shouldBe` Right (XRange Any)
      parseComp "<1.2.3 || 5" `shouldBe` Right (PrimitiveComparator LessThan (Full 1 2 3))

    it "rejects invalid formats" $ do
      isLeft (parseComp "foo") `shouldBe` True
      -- Hyphen ranges requires spaces
      isLeft (strictParseComp "1.2.3-1.2.3") `shouldBe` True

  describe "hyphenRangeComparatorParser" $ do
    let parseHyphen = P.parse hyphenRangeComparatorParser ""

    it "parses hyphen range" $ do
      parseHyphen "1.2.3 - 2.3.4" `shouldBe` Right (HyphenRange (Full 1 2 3) (Full 2 3 4))
      parseHyphen "1.2 - 2.3.4" `shouldBe` Right (HyphenRange (MajorMinor 1 2) (Full 2 3 4))
      parseHyphen "1.2.3 - 2.3" `shouldBe` Right (HyphenRange (Full 1 2 3) (MajorMinor 2 3))
      parseHyphen "1 - 3" `shouldBe` Right (HyphenRange (Major 1) (Major 3))
      parseHyphen "* - *" `shouldBe` Right (HyphenRange Any Any)

    it "parses hyphen range with trailing content" $ do
      parseHyphen "1.2.3 - 2.3.4 || something" `shouldBe` Right (HyphenRange (Full 1 2 3) (Full 2 3 4))
      parseHyphen "1.2 - 2.3.4 ^1.2.3" `shouldBe` Right (HyphenRange (MajorMinor 1 2) (Full 2 3 4))

    it "rejects invalid formats" $ do
      isLeft (parseHyphen "") `shouldBe` True
      isLeft (parseHyphen "foo") `shouldBe` True
      isLeft (parseHyphen "1.2") `shouldBe` True
      isLeft (parseHyphen "1.2 - ") `shouldBe` True
      isLeft (parseHyphen "1.2 - a") `shouldBe` True

  describe "versionBounds" $ do
    let comp ~> expectedInterval =
          it (show comp) $ versionBounds comp `shouldBe` expectedInterval

    -- Primitive: GreaterThanOrEqual
    PrimitiveComparator GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
    PrimitiveComparator GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
    PrimitiveComparator GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]
    PrimitiveComparator GreaterThanOrEqual [pv|*|] ~> allVersionsInterval

    -- Primitive: GreaterThan
    PrimitiveComparator GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
    PrimitiveComparator GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
    PrimitiveComparator GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]
    PrimitiveComparator GreaterThan [pv|*|] ~> noVersionsInterval

    -- Primitive: LessThan
    PrimitiveComparator LessThan [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3) |]
    PrimitiveComparator LessThan [pv|1.2|] ~> [vi| [0.0.0, 1.2.0) |]
    PrimitiveComparator LessThan [pv|1|] ~> [vi| [0.0.0, 1.0.0) |]
    PrimitiveComparator LessThan [pv|*|] ~> noVersionsInterval

    -- Primitive: LessThanOrEqual
    PrimitiveComparator LessThanOrEqual [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
    PrimitiveComparator LessThanOrEqual [pv|1.2|] ~> [vi| [0.0.0, 1.3.0) |]
    PrimitiveComparator LessThanOrEqual [pv|1|] ~> [vi| [0.0.0, 2.0.0) |]
    PrimitiveComparator LessThanOrEqual [pv|*|] ~> allVersionsInterval

    -- Primitive: Equal
    PrimitiveComparator Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
    PrimitiveComparator Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    PrimitiveComparator Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    PrimitiveComparator Equal [pv|*|] ~> allVersionsInterval

    -- Tilde range bounds
    ApproximatelyEquivalentTo [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
    ApproximatelyEquivalentTo [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    ApproximatelyEquivalentTo [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    ApproximatelyEquivalentTo [pv|*|] ~> allVersionsInterval

    -- Caret range bounds
    BackwardsCompatibleWith [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
    BackwardsCompatibleWith [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
    BackwardsCompatibleWith [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
    BackwardsCompatibleWith [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
    BackwardsCompatibleWith [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
    BackwardsCompatibleWith [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
    BackwardsCompatibleWith [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    BackwardsCompatibleWith [pv|0|] ~> [vi| [0.0.0, 1.0.0) |]
    BackwardsCompatibleWith [pv|*|] ~> allVersionsInterval

    -- X-range bounds
    XRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
    XRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    XRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    XRange [pv|*|] ~> allVersionsInterval

    -- Hyphen range bounds
    HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
    HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
    HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
    HyphenRange [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
    HyphenRange [pv|*|] [pv|*|] ~> allVersionsInterval
