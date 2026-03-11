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

  describe "comparatorParser" $ do
    let parseComp = P.parse primitiveComparatorParser ""
        strictParseComp = P.parse (primitiveComparatorParser <* P.eof) ""

    it "parses primitive operator comparators" $ do
      strictParseComp ">=1.2.3" `shouldBe` Right (PrimitiveComparator GreaterThanOrEqual (MajorMinorPatch 1 2 3))
      strictParseComp "<=1.2.3" `shouldBe` Right (PrimitiveComparator LessThanOrEqual (MajorMinorPatch 1 2 3))
      strictParseComp ">1.2.3" `shouldBe` Right (PrimitiveComparator GreaterThan (MajorMinorPatch 1 2 3))
      strictParseComp "<1.2.3" `shouldBe` Right (PrimitiveComparator LessThan (MajorMinorPatch 1 2 3))
      strictParseComp "=1.2.3" `shouldBe` Right (PrimitiveComparator Equal (MajorMinorPatch 1 2 3))
      strictParseComp ">=1.2" `shouldBe` Right (PrimitiveComparator GreaterThanOrEqual (MajorMinor 1 2))
      strictParseComp "<=1.2" `shouldBe` Right (PrimitiveComparator LessThanOrEqual (MajorMinor 1 2))
      strictParseComp "=1.2" `shouldBe` Right (PrimitiveComparator Equal (MajorMinor 1 2))
      strictParseComp ">1" `shouldBe` Right (PrimitiveComparator GreaterThan (Major 1))
      strictParseComp "<1" `shouldBe` Right (PrimitiveComparator LessThan (Major 1))
      strictParseComp "=*" `shouldBe` Right (PrimitiveComparator Equal Any)
      strictParseComp ">=*" `shouldBe` Right (PrimitiveComparator GreaterThanOrEqual Any)
      strictParseComp "<=*" `shouldBe` Right (PrimitiveComparator LessThanOrEqual Any)

    it "parses comparators with trailing content" $ do
      parseComp "<1.2.3 || 5" `shouldBe` Right (PrimitiveComparator LessThan (MajorMinorPatch 1 2 3))

    it "rejects invalid formats" $ do
      isLeft (strictParseComp "") `shouldBe` True
      isLeft (strictParseComp "foo") `shouldBe` True
      isLeft (strictParseComp "$1.2.3") `shouldBe` True
      isLeft (strictParseComp "?1.x.x") `shouldBe` True

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
