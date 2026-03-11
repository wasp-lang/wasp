module SemanticVersion.ComparatorSetTest where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
import Wasp.SemanticVersion.ComparatorSet
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "show" $ do
    it "shows single simple comparator set" $ do
      show
        ( SimpleComparatorSet $
            NE.fromList
              [ XRange [pv|1.2|]
              ]
        )
        `shouldBe` "1.2"
    it "shows multiple simple comparator set" $ do
      show
        ( SimpleComparatorSet $
            NE.fromList
              [ Primitive (PrimitiveComparator GreaterThanOrEqual [pv|1.2.0|]),
                Primitive (PrimitiveComparator LessThan [pv|2.0.0|]),
                Caret [pv|2.1.3|]
              ]
        )
        `shouldBe` ">=1.2.0 <2.0.0 ^2.1.3"
    it "shows hyphen range" $ do
      show (HyphenRange [pv|1.2.3|] [pv|4.5.6|]) `shouldBe` "1.2.3 - 4.5.6"
      show (HyphenRange [pv|1.2|] [pv|3|]) `shouldBe` "1.2 - 3"
      show (HyphenRange [pv|1|] [pv|2.3|]) `shouldBe` "1 - 2.3"
      show (HyphenRange [pv|*|] [pv|*|]) `shouldBe` "* - *"

  describe "show Simple" $ do
    it "primitive" $ do
      show (Primitive (PrimitiveComparator GreaterThanOrEqual [pv|1.2.3|])) `shouldBe` ">=1.2.3"
      show (Primitive (PrimitiveComparator Equal [pv|1.2.3|])) `shouldBe` "1.2.3"
    it "tilde" $ do
      show (Tilde [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (Tilde [pv|1.2|]) `shouldBe` "~1.2"
      show (Tilde [pv|1|]) `shouldBe` "~1"
      show (Tilde [pv|*|]) `shouldBe` "~*"
    it "caret" $ do
      show (Caret [pv|1.2.3|]) `shouldBe` "^1.2.3"
      show (Caret [pv|1.2|]) `shouldBe` "^1.2"
      show (Caret [pv|1|]) `shouldBe` "^1"
      show (Caret [pv|*|]) `shouldBe` "^*"
    it "x-range" $ do
      show (XRange [pv|1.2.3|]) `shouldBe` "1.2.3"
      show (XRange [pv|1.2|]) `shouldBe` "1.2"
      show (XRange [pv|1|]) `shouldBe` "1"
      show (XRange [pv|*|]) `shouldBe` "*"

  describe "simpleParser" $ do
    let parseSimple = P.parse simpleParser ""
        strictParseSimple = P.parse (simpleParser <* P.eof) ""

    it "parses primitive comparators" $ do
      strictParseSimple ">=1.2.3" `shouldBe` Right (Primitive (PrimitiveComparator GreaterThanOrEqual (MajorMinorPatch 1 2 3)))
      strictParseSimple "<=1.2.3" `shouldBe` Right (Primitive (PrimitiveComparator LessThanOrEqual (MajorMinorPatch 1 2 3)))
      strictParseSimple ">1.2.3" `shouldBe` Right (Primitive (PrimitiveComparator GreaterThan (MajorMinorPatch 1 2 3)))
      strictParseSimple "<1.2.3" `shouldBe` Right (Primitive (PrimitiveComparator LessThan (MajorMinorPatch 1 2 3)))
      strictParseSimple "=1.2.3" `shouldBe` Right (Primitive (PrimitiveComparator Equal (MajorMinorPatch 1 2 3)))

    it "parses caret comparators" $ do
      strictParseSimple "^1.2.3" `shouldBe` Right (Caret (MajorMinorPatch 1 2 3))
      strictParseSimple "^1.2" `shouldBe` Right (Caret (MajorMinor 1 2))
      strictParseSimple "^1" `shouldBe` Right (Caret (Major 1))
      strictParseSimple "^*" `shouldBe` Right (Caret Any)

    it "parses tilde comparators" $ do
      strictParseSimple "~1.2.3" `shouldBe` Right (Tilde (MajorMinorPatch 1 2 3))
      strictParseSimple "~1.2" `shouldBe` Right (Tilde (MajorMinor 1 2))
      strictParseSimple "~1" `shouldBe` Right (Tilde (Major 1))
      strictParseSimple "~*" `shouldBe` Right (Tilde Any)

    it "parses x-range comparators" $ do
      strictParseSimple "1.2.3" `shouldBe` Right (XRange (MajorMinorPatch 1 2 3))
      strictParseSimple "1.2.x" `shouldBe` Right (XRange (MajorMinor 1 2))
      strictParseSimple "1.x.x" `shouldBe` Right (XRange (Major 1))
      strictParseSimple "1.2" `shouldBe` Right (XRange (MajorMinor 1 2))
      strictParseSimple "1.x" `shouldBe` Right (XRange (Major 1))
      strictParseSimple "1" `shouldBe` Right (XRange (Major 1))
      strictParseSimple "X" `shouldBe` Right (XRange Any)
      strictParseSimple "x" `shouldBe` Right (XRange Any)
      strictParseSimple "*" `shouldBe` Right (XRange Any)

    it "parses simple comparators with trailing content" $ do
      parseSimple "* 1.2.3" `shouldBe` Right (XRange Any)
      parseSimple "<1.2.3 || 5" `shouldBe` Right (Primitive (PrimitiveComparator LessThan (MajorMinorPatch 1 2 3)))

    it "rejects invalid formats" $ do
      isLeft (strictParseSimple "") `shouldBe` True
      isLeft (strictParseSimple "foo") `shouldBe` True
      isLeft (strictParseSimple "$1.2.3") `shouldBe` True
      isLeft (strictParseSimple "?1.x.x") `shouldBe` True

  describe "hyphenRangeParser" $ do
    let parseHyphenRange = P.parse hyphenRangeParser ""
        strictParseHyphenRange = P.parse (hyphenRangeParser <* P.eof) ""

    it "parses hyphen range" $ do
      strictParseHyphenRange "1.2.3 - 2.3.4" `shouldBe` Right (HyphenRange (MajorMinorPatch 1 2 3) (MajorMinorPatch 2 3 4))
      strictParseHyphenRange "1.2 - 2.3.4" `shouldBe` Right (HyphenRange (MajorMinor 1 2) (MajorMinorPatch 2 3 4))
      strictParseHyphenRange "1.2.3 - 2.3" `shouldBe` Right (HyphenRange (MajorMinorPatch 1 2 3) (MajorMinor 2 3))
      strictParseHyphenRange "1.2 - 3.4" `shouldBe` Right (HyphenRange (MajorMinor 1 2) (MajorMinor 3 4))
      strictParseHyphenRange "1 - 3" `shouldBe` Right (HyphenRange (Major 1) (Major 3))
      strictParseHyphenRange "* - *" `shouldBe` Right (HyphenRange Any Any)

    it "parses hyphen range with trailing content" $ do
      parseHyphenRange "1.2.3 - 2.3.4 || something" `shouldBe` Right (HyphenRange (MajorMinorPatch 1 2 3) (MajorMinorPatch 2 3 4))
      parseHyphenRange "1.2 - 2.3.4 ^1.2.3" `shouldBe` Right (HyphenRange (MajorMinor 1 2) (MajorMinorPatch 2 3 4))

    it "rejects invalid formats" $ do
      isLeft (strictParseHyphenRange "") `shouldBe` True
      isLeft (strictParseHyphenRange "foo") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2 - ") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2 - a") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2 -  3.4") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2  - 3.4") `shouldBe` True
      isLeft (strictParseHyphenRange "1.2-3.4") `shouldBe` True

  describe "comparatorSetParser" $ do
    let parseCompSet = P.parse comparatorSetParser ""
        strictParseCompSet = P.parse (comparatorSetParser <* P.eof) ""

    it "parses comparator sets with single comparator" $ do
      strictParseCompSet ">=1.0.0"
        `shouldBe` Right
          ( SimpleComparatorSet $
              NE.fromList
                [ Primitive (PrimitiveComparator GreaterThanOrEqual (MajorMinorPatch 1 0 0))
                ]
          )

    it "parses comparator sets with multiple comparators" $ do
      strictParseCompSet ">=1.0.0 <2.0.0"
        `shouldBe` Right
          ( SimpleComparatorSet $
              NE.fromList
                [ Primitive (PrimitiveComparator GreaterThanOrEqual (MajorMinorPatch 1 0 0)),
                  Primitive (PrimitiveComparator LessThan (MajorMinorPatch 2 0 0))
                ]
          )
      strictParseCompSet ">1.0.0    <=2.0.0      ^1.2  * ~0.1.X"
        `shouldBe` Right
          ( SimpleComparatorSet $
              NE.fromList
                [ Primitive (PrimitiveComparator GreaterThan (MajorMinorPatch 1 0 0)),
                  Primitive (PrimitiveComparator LessThanOrEqual (MajorMinorPatch 2 0 0)),
                  Caret (MajorMinor 1 2),
                  XRange Any,
                  Tilde (MajorMinor 0 1)
                ]
          )

    it "parses comparator sets with trailing content" $ do
      parseCompSet ">=1.0.0 <2.0.0 || 1"
        `shouldBe` Right
          ( SimpleComparatorSet $
              NE.fromList
                [ Primitive (PrimitiveComparator GreaterThanOrEqual (MajorMinorPatch 1 0 0)),
                  Primitive (PrimitiveComparator LessThan (MajorMinorPatch 2 0 0))
                ]
          )
      parseCompSet ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
        `shouldBe` Right
          ( SimpleComparatorSet $
              NE.fromList
                [ Primitive (PrimitiveComparator GreaterThan (MajorMinorPatch 1 0 0)),
                  Primitive (PrimitiveComparator LessThanOrEqual (MajorMinorPatch 2 0 0)),
                  Caret (MajorMinor 1 2),
                  XRange Any,
                  Tilde (MajorMinor 0 1)
                ]
          )

    it "rejects invalid formats" $ do
      isLeft (strictParseCompSet "") `shouldBe` True
      isLeft (strictParseCompSet "foo") `shouldBe` True
      isLeft (strictParseCompSet ">1<2") `shouldBe` True

    describe "hyphen ranges cannot be combined with other comparators" $ do
      -- We must use a strict parser here because "1.2.3 - 2.0.0 3.x" will be parsed
      -- as "1.2.3 - 2.0.0" hyphen range comparator set with " 3.x" suffix.

      it "parses hyphen ranges as sole comparator in a set" $ do
        isRight (strictParseCompSet "1.2.3 - 2.0.0") `shouldBe` True
        isRight (strictParseCompSet "1 - 3") `shouldBe` True

      it "rejects comparator sets that mix a hyphen range with other comparators" $ do
        isLeft (strictParseCompSet ">=1.2.3 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet ">1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "<2.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 >=3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 <3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "^1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "~1.0.0 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 ^3.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.x 1.2.3 - 2.0.0") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 3.x") `shouldBe` True
        isLeft (strictParseCompSet "1.2.3 - 2.0.0 1.2.3 - 2.0.0") `shouldBe` True

  describe "versionBounds Simple" $ do
    let simple ~> expectedInterval =
          it (show simple) $ versionBounds simple `shouldBe` expectedInterval

    -- Tilde range bounds
    Tilde [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
    Tilde [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    Tilde [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    Tilde [pv|*|] ~> allVersionsInterval

    -- Caret range bounds
    Caret [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
    Caret [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
    Caret [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
    Caret [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
    Caret [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
    Caret [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
    Caret [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    Caret [pv|0|] ~> [vi| [0.0.0, 1.0.0) |]
    Caret [pv|*|] ~> allVersionsInterval

    -- X-range bounds
    XRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
    XRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
    XRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
    XRange [pv|*|] ~> allVersionsInterval

  -- Hyphen range bounds
  describe "versionBounds HyphenRange" $ do
    let range ~> expectedInterval =
          it (show range) $ versionBounds range `shouldBe` expectedInterval

    HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
    HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
    HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
    HyphenRange [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
    HyphenRange [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
    HyphenRange [pv|*|] [pv|*|] ~> allVersionsInterval

  -- Just does 'intervalIntersection' under the hood.
  describe "versionBounds ComparatorSet" $ do
    let comps ~> expectedInterval =
          let compSet = SimpleComparatorSet (NE.fromList comps)
           in it (show compSet) $ versionBounds compSet `shouldBe` expectedInterval

    -- Basic inclusive/exclusive combinations
    [Primitive (PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|]), Primitive (PrimitiveComparator LessThan [pv|2.0.0|])]
      ~> [vi| [1.0.0, 2.0.0) |]
    [Primitive (PrimitiveComparator GreaterThan [pv|1.0.0|]), Primitive (PrimitiveComparator LessThanOrEqual [pv|2.0.0|])]
      ~> [vi| (1.0.0, 2.0.0] |]

    -- Caret narrows an open upper bound
    [Primitive (PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|]), Caret [pv|1.2.0|]]
      ~> [vi| [1.2.0, 2.0.0) |]

    -- Tilde narrows further than caret
    [Caret [pv|1.0.0|], Tilde [pv|1.2.0|]]
      ~> [vi| [1.2.0, 1.3.0) |]

    -- Three comparators: the tightest wins on each side
    [Primitive (PrimitiveComparator GreaterThan [pv|0.5.0|]), Primitive (PrimitiveComparator LessThan [pv|3.0.0|]), Caret [pv|1.0.0|]]
      ~> [vi| [1.0.0, 2.0.0) |]

    -- Exclusive beats inclusive at the same version
    [Primitive (PrimitiveComparator GreaterThan [pv|1.0.0|]), Primitive (PrimitiveComparator GreaterThanOrEqual [pv|1.0.0|])]
      ~> [vi| (1.0.0, inf) |]
    [Primitive (PrimitiveComparator LessThan [pv|2.0.0|]), Primitive (PrimitiveComparator LessThanOrEqual [pv|2.0.0|])]
      ~> [vi| [0.0.0, 2.0.0) |]
