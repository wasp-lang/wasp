module SemanticVersion.VersionBoundTest where

import Test.Tasty.Hspec
import Wasp.SemanticVersion.Version
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersionBound :: Spec
spec_SemanticVersionBound = do
  it "showInterval" $ do
    showInterval (Inclusive (Version 1 2 3), Exclusive (Version 2 3 4)) `shouldBe` "[1.2.3, 2.3.4)"
    showInterval (Exclusive (Version 1 2 3), Inclusive (Version 2 3 4)) `shouldBe` "(1.2.3, 2.3.4]"
    showInterval (Inf, Inf) `shouldBe` "(inf, inf)"

  it "parseInterval" $ do
    parseInterval "[1.2.3, 0.1)" `shouldBe` Right (Inclusive (Version 1 2 3), Exclusive (Version 0 1 0))
    parseInterval "(1,inf)" `shouldBe` Right (Exclusive (Version 1 0 0), Inf)
    parseInterval "  ( inf , 1.2.3 ] " `shouldBe` Right (Inf, Inclusive (Version 1 2 3))

  it "vi quasi quoter" $ do
    [vi| [1.2.3, 2.3.4) |] `shouldBe` (Inclusive (Version 1 2 3), Exclusive (Version 2 3 4))
    [vi| (1.2.3, 2.3.4] |] `shouldBe` (Exclusive (Version 1 2 3), Inclusive (Version 2 3 4))
    [vi| (inf, inf) |] `shouldBe` (Inf, Inf)

  describe "intervalIntersection" $ do
    let i1 ∩ i2 = \i3 ->
          it (showInterval i1 <> " ∩ " <> showInterval i2 <> " == " <> showInterval i3) $
            i1 `intervalIntersection` i2 `shouldBe` i3

    [vi| [3, inf) |] ∩ [vi| [4, inf) |] ~> [vi| [4, inf) |]
    [vi| (3, inf) |] ∩ [vi| [3, inf) |] ~> [vi| (3, inf) |]
    [vi| (inf, 1] |] ∩ [vi| (0.1, inf) |] ~> [vi| (0.1, 1] |]

  describe "intervalUnion" $ do
    let i1 ∪ i2 = \i3 ->
          it (showInterval i1 <> " ∪ " <> showInterval i2 <> " == " <> showInterval i3) $
            i1 `intervalUnion` i2 `shouldBe` i3

    [vi| [3, inf) |] ∪ [vi| [4, inf) |] ~> [vi| [3, inf) |]
    [vi| (3, inf) |] ∪ [vi| [3, inf) |] ~> [vi| [3, inf) |]
    [vi| (inf, 1] |] ∪ [vi| (0.1, inf) |] ~> [vi| (inf, inf) |]

  describe "isSubintervalOf" $ do
    let i1 ⊆ i2 = \r ->
          it (showInterval i1 <> " ⊆ " <> showInterval i2 <> " == " <> show r) $
            i1 `isSubintervalOf` i2 `shouldBe` r

    [vi| [4, inf) |] ⊆ [vi| [3, inf) |] ~> True
    [vi| (3, inf) |] ⊆ [vi| [3, inf) |] ~> True
    [vi| [3, inf) |] ⊆ [vi| (3, inf) |] ~> False
    [vi| (inf, 1] |] ⊆ [vi| (0.1, inf) |] ~> False
    [vi| (inf, inf) |] ⊆ [vi| (inf, inf) |] ~> True
    [vi| [2, 2.5) |] ⊆ [vi| (1, 2.6] |] ~> True

  describe "isVersionInInterval" $ do
    let version ∈ i = \r ->
          it (showInterval i <> " ∈ " <> show version <> " == " <> show r) $
            isVersionInInterval i version `shouldBe` r

    [v|1.2.3|] ∈ [vi| [4, inf) |] ~> False
    [v|1.2.3|] ∈ [vi| (inf, inf) |] ~> True
    [v|1.2.3|] ∈ [vi| [1, 2) |] ~> True
    [v|1.2.3|] ∈ [vi| [1.2.3, 1.2.3] |] ~> True
    [v|1.2.3|] ∈ [vi| (1.2.3, 1.5.0] |] ~> False
  where
    (~>) = ($)
