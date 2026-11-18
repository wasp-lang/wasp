module SemanticVersion.VersionBoundTest where

import Test.Hspec
import Wasp.SemanticVersion.Version
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersionBound :: Spec
spec_SemanticVersionBound = do
  it "showInterval" $ do
    showInterval (Inclusive [v|1.2.3|], Exclusive [v|2.3.4|]) `shouldBe` "[1.2.3, 2.3.4)"
    showInterval (Exclusive [v|1.2.3|], Inclusive [v|2.3.4|]) `shouldBe` "(1.2.3, 2.3.4]"
    showInterval (Inf, Inf) `shouldBe` "(inf, inf)"
    showInterval (Inclusive [v|1.2.3-alpha.1|], Exclusive [v|2.0.0-0|]) `shouldBe` "[1.2.3-alpha.1, 2.0.0-0)"

  it "parseInterval" $ do
    parseInterval "[1.2.3, 0.1.0)" `shouldBe` Right (Inclusive [v|1.2.3|], Exclusive [v|0.1.0|])
    parseInterval "(1.0.0,inf)" `shouldBe` Right (Exclusive [v|1.0.0|], Inf)
    parseInterval "  ( inf , 1.2.3 ] " `shouldBe` Right (Inf, Inclusive [v|1.2.3|])
    parseInterval "[1.2.3-alpha.1, 2.0.0-0)" `shouldBe` Right (Inclusive [v|1.2.3-alpha.1|], Exclusive [v|2.0.0-0|])
    parseInterval "(1.2.3-rc.1+build, 2.0.0-0]" `shouldBe` Right (Exclusive [v|1.2.3-rc.1|], Inclusive [v|2.0.0-0|])

  it "vi quasi quoter" $ do
    [vi| [1.2.3, 2.3.4) |] `shouldBe` (Inclusive [v|1.2.3|], Exclusive [v|2.3.4|])
    [vi| [1.2.3-alpha, 2.0.0-0) |] `shouldBe` (Inclusive [v|1.2.3-alpha|], Exclusive [v|2.0.0-0|])
    [vi| (1.2.3, 2.3.4] |] `shouldBe` (Exclusive [v|1.2.3|], Inclusive [v|2.3.4|])
    [vi| (inf, inf) |] `shouldBe` (Inf, Inf)

  it "allVersionsInterval" $ do
    allVersionsInterval `shouldBe` [vi| [0.0.0-0, inf) |]

  describe "versionFromBound" $ do
    it "returns the version of a bound" $ do
      versionFromBound (Inclusive [v|1.2.3-alpha|]) `shouldBe` Just [v|1.2.3-alpha|]
      versionFromBound (Exclusive [v|1.2.3|]) `shouldBe` Just [v|1.2.3|]
      versionFromBound Inf `shouldBe` Nothing

  describe "intervalIntersection" $ do
    let i1 ∩ i2 = \i3 ->
          it (showInterval i1 <> " ∩ " <> showInterval i2 <> " == " <> showInterval i3) $
            i1 `intervalIntersection` i2 `shouldBe` i3

    [vi| [3.0.0, inf) |] ∩ [vi| [4.0.0, inf) |] ~> [vi| [4.0.0, inf) |]
    [vi| (3.0.0, inf) |] ∩ [vi| [3.0.0, inf) |] ~> [vi| (3.0.0, inf) |]
    [vi| (inf, 1.0.0] |] ∩ [vi| (0.1.0, inf) |] ~> [vi| (0.1.0, 1.0.0] |]
    [vi| [1.0.0-alpha, inf) |] ∩ [vi| [1.0.0-beta, inf) |] ~> [vi| [1.0.0-beta, inf) |]
    [vi| [1.0.0-alpha, inf) |] ∩ [vi| [1.0.0, inf) |] ~> [vi| [1.0.0, inf) |]
    [vi| [0.0.0-0, 2.0.0) |] ∩ [vi| [1.0.0, 2.0.0-0) |] ~> [vi| [1.0.0, 2.0.0-0) |]

  describe "intervalUnion" $ do
    let i1 ∪ i2 = \i3 ->
          it (showInterval i1 <> " ∪ " <> showInterval i2 <> " == " <> showInterval i3) $
            i1 `intervalUnion` i2 `shouldBe` i3

    [vi| [3.0.0, inf) |] ∪ [vi| [4.0.0, inf) |] ~> [vi| [3.0.0, inf) |]
    [vi| (3.0.0, inf) |] ∪ [vi| [3.0.0, inf) |] ~> [vi| [3.0.0, inf) |]
    [vi| (inf, 1.0.0] |] ∪ [vi| (0.1.0, inf) |] ~> [vi| (inf, inf) |]
    [vi| [1.0.0-alpha, inf) |] ∪ [vi| [1.0.0-beta, inf) |] ~> [vi| [1.0.0-alpha, inf) |]
    [vi| [0.0.0-0, 2.0.0) |] ∪ [vi| [1.0.0, 2.0.0-0) |] ~> [vi| [0.0.0-0, 2.0.0) |]

  describe "isSubintervalOf" $ do
    let i1 ⊆ i2 = \r ->
          it (showInterval i1 <> " ⊆ " <> showInterval i2 <> " == " <> show r) $
            i1 `isSubintervalOf` i2 `shouldBe` r

    [vi| [4.0.0, inf) |] ⊆ [vi| [3.0.0, inf) |] ~> True
    [vi| (3.0.0, inf) |] ⊆ [vi| [3.0.0, inf) |] ~> True
    [vi| [3.0.0, inf) |] ⊆ [vi| (3.0.0, inf) |] ~> False
    [vi| (inf, 1.0.0] |] ⊆ [vi| (0.1.0, inf) |] ~> False
    [vi| (inf, inf) |] ⊆ [vi| (inf, inf) |] ~> True
    [vi| [2.0.0, 2.5.0) |] ⊆ [vi| (1.0.0, 2.6.0] |] ~> True
    [vi| [1.0.0, 2.0.0-0) |] ⊆ [vi| [1.0.0, 2.0.0) |] ~> True
    [vi| [1.0.0, 2.0.0) |] ⊆ [vi| [1.0.0, 2.0.0-0) |] ~> False
    [vi| [1.0.0-alpha, 2.0.0) |] ⊆ [vi| [1.0.0, 2.0.0) |] ~> False
    [vi| [1.0.0, 2.0.0) |] ⊆ [vi| [1.0.0-alpha, 2.0.0) |] ~> True

  describe "isVersionInInterval" $ do
    let version ∈ i = \r ->
          it (showInterval i <> " ∈ " <> show version <> " == " <> show r) $
            isVersionInInterval i version `shouldBe` r

    [v|1.2.3|] ∈ [vi| [4.0.0, inf) |] ~> False
    [v|1.2.3|] ∈ [vi| (inf, inf) |] ~> True
    [v|1.2.3|] ∈ [vi| [1.0.0, 2.0.0) |] ~> True
    [v|1.2.3|] ∈ [vi| [1.2.3, 1.2.3] |] ~> True
    [v|1.2.3|] ∈ [vi| (1.2.3, 1.5.0] |] ~> False
    [v|1.2.3-alpha|] ∈ [vi| [1.2.3, 1.5.0] |] ~> False
    [v|1.2.3-alpha|] ∈ [vi| [1.2.3-alpha, 1.5.0] |] ~> True
    [v|1.2.3-beta|] ∈ [vi| [1.2.3-alpha, 1.5.0] |] ~> True
    [v|2.0.0-alpha|] ∈ [vi| [1.0.0, 2.0.0) |] ~> True
    [v|2.0.0-alpha|] ∈ [vi| [1.0.0, 2.0.0-0) |] ~> False
    [v|2.0.0-0|] ∈ [vi| [1.0.0, 2.0.0-0] |] ~> True
    [v|0.0.0-0|] ∈ allVersionsInterval ~> True
    [v|1.2.3+build|] ∈ [vi| [1.2.3, 1.2.3] |] ~> True
    [v|1.2.3+build|] ∈ [vi| (1.2.3, 1.5.0] |] ~> False
    [v|1.2.3-alpha+build|] ∈ [vi| [1.2.3-alpha, 1.2.3-alpha] |] ~> True
  where
    (~>) = ($)
