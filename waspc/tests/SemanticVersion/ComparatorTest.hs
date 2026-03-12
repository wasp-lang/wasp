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
      show (Comparator GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (Comparator GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (Comparator LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (Comparator LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (Comparator Equal [pv|1.2.3|]) `shouldBe` "1.2.3"

  describe "comparatorParser" $ do
    let parseComp = P.parse primitiveComparatorParser ""
        strictParseComp = P.parse (primitiveComparatorParser <* P.eof) ""

    it "parses comparators" $ do
      strictParseComp "=1.2.3" `shouldBe` Right (Comparator Equal [pv|1.2.3|])
      strictParseComp "=1.2.x" `shouldBe` Right (Comparator Equal [pv|1.2|])
      strictParseComp "=1.x.x" `shouldBe` Right (Comparator Equal [pv|1|])
      strictParseComp "=1.2" `shouldBe` Right (Comparator Equal [pv|1.2|])
      strictParseComp "=1.x" `shouldBe` Right (Comparator Equal [pv|1|])
      strictParseComp "=1" `shouldBe` Right (Comparator Equal [pv|1|])
      strictParseComp "=X" `shouldBe` Right (Comparator Equal [pv|*|])
      strictParseComp "=x" `shouldBe` Right (Comparator Equal [pv|*|])
      strictParseComp "=*" `shouldBe` Right (Comparator Equal [pv|*|])

      strictParseComp ">1.2.3" `shouldBe` Right (Comparator GreaterThan [pv|1.2.3|])
      strictParseComp ">1.2.x" `shouldBe` Right (Comparator GreaterThan [pv|1.2|])
      strictParseComp ">1.x.x" `shouldBe` Right (Comparator GreaterThan [pv|1|])
      strictParseComp ">1.2" `shouldBe` Right (Comparator GreaterThan [pv|1.2|])
      strictParseComp ">1.x" `shouldBe` Right (Comparator GreaterThan [pv|1|])
      strictParseComp ">1" `shouldBe` Right (Comparator GreaterThan [pv|1|])
      strictParseComp ">X" `shouldBe` Right (Comparator GreaterThan [pv|*|])
      strictParseComp ">x" `shouldBe` Right (Comparator GreaterThan [pv|*|])
      strictParseComp ">*" `shouldBe` Right (Comparator GreaterThan [pv|*|])

      strictParseComp "<1.2.3" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])
      strictParseComp "<1.2.x" `shouldBe` Right (Comparator LessThan [pv|1.2|])
      strictParseComp "<1.x.x" `shouldBe` Right (Comparator LessThan [pv|1|])
      strictParseComp "<1.2" `shouldBe` Right (Comparator LessThan [pv|1.2|])
      strictParseComp "<1.x" `shouldBe` Right (Comparator LessThan [pv|1|])
      strictParseComp "<1" `shouldBe` Right (Comparator LessThan [pv|1|])
      strictParseComp "<X" `shouldBe` Right (Comparator LessThan [pv|*|])
      strictParseComp "<x" `shouldBe` Right (Comparator LessThan [pv|*|])
      strictParseComp "<*" `shouldBe` Right (Comparator LessThan [pv|*|])

      strictParseComp ">=1.2.3" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1.2.3|])
      strictParseComp ">=1.2.x" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1.2|])
      strictParseComp ">=1.x.x" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1|])
      strictParseComp ">=1.2" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1.2|])
      strictParseComp ">=1.x" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1|])
      strictParseComp ">=1" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1|])
      strictParseComp ">=X" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|*|])
      strictParseComp ">=x" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|*|])
      strictParseComp ">=*" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|*|])

      strictParseComp "<=1.2.3" `shouldBe` Right (Comparator LessThanOrEqual [pv|1.2.3|])
      strictParseComp "<=1.2.x" `shouldBe` Right (Comparator LessThanOrEqual [pv|1.2|])
      strictParseComp "<=1.x.x" `shouldBe` Right (Comparator LessThanOrEqual [pv|1|])
      strictParseComp "<=1.2" `shouldBe` Right (Comparator LessThanOrEqual [pv|1.2|])
      strictParseComp "<=1.x" `shouldBe` Right (Comparator LessThanOrEqual [pv|1|])
      strictParseComp "<=1" `shouldBe` Right (Comparator LessThanOrEqual [pv|1|])
      strictParseComp "<=X" `shouldBe` Right (Comparator LessThanOrEqual [pv|*|])
      strictParseComp "<=x" `shouldBe` Right (Comparator LessThanOrEqual [pv|*|])
      strictParseComp "<=*" `shouldBe` Right (Comparator LessThanOrEqual [pv|*|])

    it "parses comparators with trailing content" $ do
      parseComp "<1.2.3 || 5" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])

    it "rejects invalid formats" $ do
      isLeft (strictParseComp "") `shouldBe` True
      isLeft (strictParseComp "foo") `shouldBe` True
      isLeft (strictParseComp "$1.2.3") `shouldBe` True
      isLeft (strictParseComp "?1.x.x") `shouldBe` True

  describe "versionBounds" $ do
    let comp ~> expectedInterval =
          it (show comp) $ versionBounds comp `shouldBe` expectedInterval

    -- Primitive: GreaterThanOrEqual
    Comparator GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
    Comparator GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
    Comparator GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]
    Comparator GreaterThanOrEqual [pv|*|] ~> allVersionsInterval

    -- Primitive: GreaterThan
    Comparator GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
    Comparator GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
    Comparator GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]
    Comparator GreaterThan [pv|*|] ~> noVersionInterval

    -- Primitive: LessThan
    Comparator LessThan [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3) |]
    Comparator LessThan [pv|1.2|] ~> [vi| [0.0.0, 1.2.0) |]
    Comparator LessThan [pv|1|] ~> [vi| [0.0.0, 1.0.0) |]
    Comparator LessThan [pv|*|] ~> noVersionInterval

    -- Primitive: LessThanOrEqual
    Comparator LessThanOrEqual [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
    Comparator LessThanOrEqual [pv|1.2|] ~> [vi| [0.0.0, 1.3.0) |]
    Comparator LessThanOrEqual [pv|1|] ~> [vi| [0.0.0, 2.0.0) |]
    Comparator LessThanOrEqual [pv|*|] ~> allVersionsInterval

    -- Primitive: Equal
    Comparator Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
    Comparator Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    Comparator Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    Comparator Equal [pv|*|] ~> allVersionsInterval
