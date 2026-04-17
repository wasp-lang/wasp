module SemanticVersion.ComparatorSetTest where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.ComparatorSet
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_ComparatorSet :: Spec
spec_SemanticVersion_ComparatorSet = do
  describe "ComaparatorSet" $ do
    it "show" $ do
      show
        ( SimpleComparatorSet $
            NE.fromList
              [ Primitive (Comparator Equal [pv|1.2|])
              ]
        )
        `shouldBe` "1.2"
      show
        ( SimpleComparatorSet $
            NE.fromList
              [ Primitive (Comparator GreaterThanOrEqual [pv|1.2.0|]),
                Primitive (Comparator LessThan [pv|2.0.0|]),
                TildeRange [pv|1.2.3|],
                CaretRange [pv|2.1.3|]
              ]
        )
        `shouldBe` ">=1.2.0 <2.0.0 ~1.2.3 ^2.1.3"
      show (HyphenRange [pv|1.2.3|] [pv|4.5.6|]) `shouldBe` "1.2.3 - 4.5.6"
      show (HyphenRange [pv|1.2|] [pv|3|]) `shouldBe` "1.2 - 3"
      show (HyphenRange [pv|1|] [pv|2.3|]) `shouldBe` "1 - 2.3"
      show (HyphenRange [pv|*|] [pv|*|]) `shouldBe` "* - *"

    describe "comparatorSetParser" $ do
      let parseCompSet = P.parse comparatorSetParser ""
          strictParseCompSet = P.parse (comparatorSetParser <* P.eof) ""

      it "parses comparator sets with single comparator" $ do
        strictParseCompSet ">=1.2.3"
          `shouldBe` Right
            ( SimpleComparatorSet $
                NE.fromList
                  [ Primitive (Comparator GreaterThanOrEqual [pv|1.2.3|])
                  ]
            )
        strictParseCompSet "1 - 3" `shouldBe` Right (HyphenRange [pv|1|] [pv|3|])
      it "parses comparator sets with multiple comparators" $ do
        strictParseCompSet ">=1.2.3 <1.2.3"
          `shouldBe` Right
            ( SimpleComparatorSet $
                NE.fromList
                  [ Primitive (Comparator GreaterThanOrEqual [pv|1.2.3|]),
                    Primitive (Comparator LessThan [pv|1.2.3|])
                  ]
            )
        strictParseCompSet ">1.2.3    <=1.2.3      ^1.2  * ~0.1.X"
          `shouldBe` Right
            ( SimpleComparatorSet $
                NE.fromList
                  [ Primitive (Comparator GreaterThan [pv|1.2.3|]),
                    Primitive (Comparator LessThanOrEqual [pv|1.2.3|]),
                    CaretRange [pv|1.2|],
                    Primitive (Comparator Equal [pv|*|]),
                    TildeRange [pv|0.1.X|]
                  ]
            )
      it "parses comparator sets with trailing content" $ do
        parseCompSet ">=1.2.3 <1.2.3 || 1"
          `shouldBe` Right
            ( SimpleComparatorSet $
                NE.fromList
                  [ Primitive (Comparator GreaterThanOrEqual [pv|1.2.3|]),
                    Primitive (Comparator LessThan [pv|1.2.3|])
                  ]
            )
        parseCompSet ">1.0.0 <=2.0.0 ^1.2 * ~0.1.X abc"
          `shouldBe` Right
            ( SimpleComparatorSet $
                NE.fromList
                  [ Primitive (Comparator GreaterThan [pv|1.0.0|]),
                    Primitive (Comparator LessThanOrEqual [pv|2.0.0|]),
                    CaretRange [pv|1.2|],
                    Primitive (Comparator Equal [pv|*|]),
                    TildeRange [pv|0.1.X|]
                  ]
            )
      it "rejects invalid formats" $ do
        isLeft (strictParseCompSet "") `shouldBe` True
        isLeft (strictParseCompSet "foo") `shouldBe` True
        isLeft (strictParseCompSet ">1<2") `shouldBe` True

      describe "hyphen ranges cannot be combined with other comparators" $ do
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

    describe "hyphenRangeParser" $ do
      let parseHyphenRange = P.parse hyphenRangeParser ""
          strictParseHyphenRange = P.parse (hyphenRangeParser <* P.eof) ""

      it "parses hyphen range" $ do
        strictParseHyphenRange "1.2.3 - 1.2.3" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
        strictParseHyphenRange "1.2 - 1.2" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2|])
        strictParseHyphenRange "1 - 1" `shouldBe` Right (HyphenRange [pv|1|] [pv|1|])
        strictParseHyphenRange "* - *" `shouldBe` Right (HyphenRange [pv|*|] [pv|*|])

      it "parses hyphen range with trailing content" $ do
        parseHyphenRange "1.2.3 - 1.2.3 || something" `shouldBe` Right (HyphenRange [pv|1.2.3|] [pv|1.2.3|])
        parseHyphenRange "1.2 - 1.2.3 ^1.2.3" `shouldBe` Right (HyphenRange [pv|1.2|] [pv|1.2.3|])

      it "rejects invalid formats" $ do
        isLeft (strictParseHyphenRange "") `shouldBe` True
        isLeft (strictParseHyphenRange "foo") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2 - ") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2 - a") `shouldBe` True
        -- It must be exactly " - " string between two versions
        isLeft (strictParseHyphenRange "1.2 -  1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2  - 1.2") `shouldBe` True
        isLeft (strictParseHyphenRange "1.2-1.2") `shouldBe` True

    describe "versionBounds" $ do
      let simple ~> expectedInterval =
            it (show simple) $ versionBounds simple `shouldBe` expectedInterval

      -- SimpleComparatorSet bounds
      -- Just does 'intervalIntersection' under the hood.

      -- Basic inclusive/exclusive combinations
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator GreaterThanOrEqual [pv|1.0.0|]),
            Primitive (Comparator LessThan [pv|2.0.0|])
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator GreaterThan [pv|1.0.0|]),
            Primitive (Comparator LessThanOrEqual [pv|2.0.0|])
          ]
        )
        ~> [vi| (1.0.0, 2.0.0] |]
      -- Caret narrows an open upper bound
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator GreaterThanOrEqual [pv|1.0.0|]),
            CaretRange [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 2.0.0) |]
      -- Tilde narrows further than caret
      ( SimpleComparatorSet . NE.fromList $
          [ CaretRange [pv|1.0.0|],
            TildeRange [pv|1.2.0|]
          ]
        )
        ~> [vi| [1.2.0, 1.3.0) |]
      -- Three comparators: the tightest wins on each side
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator GreaterThan [pv|0.5.0|]),
            Primitive (Comparator LessThan [pv|3.0.0|]),
            CaretRange [pv|1.0.0|]
          ]
        )
        ~> [vi| [1.0.0, 2.0.0) |]
      -- Exclusive beats inclusive at the same version
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator GreaterThan [pv|1.0.0|]),
            Primitive (Comparator GreaterThanOrEqual [pv|1.0.0|])
          ]
        )
        ~> [vi| (1.0.0, inf) |]
      ( SimpleComparatorSet . NE.fromList $
          [ Primitive (Comparator LessThan [pv|2.0.0|]),
            Primitive (Comparator LessThanOrEqual [pv|2.0.0|])
          ]
        )
        ~> [vi| [0.0.0, 2.0.0) |]

      -- HyphenRange bounds
      HyphenRange [pv|1.2.3|] [pv|2.3.4|] ~> [vi| [1.2.3, 2.3.4] |]
      HyphenRange [pv|1.2|] [pv|2.3.4|] ~> [vi| [1.2.0, 2.3.4] |]
      HyphenRange [pv|1.2.3|] [pv|2.3|] ~> [vi| [1.2.3, 2.4.0) |]
      HyphenRange [pv|1.2.3|] [pv|2|] ~> [vi| [1.2.3, 3.0.0) |]
      HyphenRange [pv|*|] [pv|2.3.4|] ~> [vi| [0.0.0, 2.3.4] |]
      HyphenRange [pv|1.2.3|] [pv|*|] ~> [vi| [1.2.3, inf) |]
      HyphenRange [pv|*|] [pv|*|] ~> allVersionsInterval

  describe "SimpleRangeExpression" $ do
    it "show" $ do
      show (Primitive $ Comparator GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (TildeRange [pv|1.2.3|]) `shouldBe` "~1.2.3"
      show (CaretRange [pv|1.2.3|]) `shouldBe` "^1.2.3"

    describe "simpleRangeExpressionParser" $ do
      let parseSimple = P.parse simpleRangeExpressionParser ""
          strictParseSimple = P.parse (simpleRangeExpressionParser <* P.eof) ""

      it "parses primitive comparators" $ do
        strictParseSimple ">=1.2.3" `shouldBe` Right (Primitive (Comparator GreaterThanOrEqual [pv|1.2.3|]))
        strictParseSimple "<=1.2.3" `shouldBe` Right (Primitive (Comparator LessThanOrEqual [pv|1.2.3|]))
        strictParseSimple ">1.2.3" `shouldBe` Right (Primitive (Comparator GreaterThan [pv|1.2.3|]))
        strictParseSimple "<1.2.3" `shouldBe` Right (Primitive (Comparator LessThan [pv|1.2.3|]))
        strictParseSimple "=1.2.3" `shouldBe` Right (Primitive (Comparator Equal [pv|1.2.3|]))

      it "parses caret comparators" $ do
        strictParseSimple "^1.2.3" `shouldBe` Right (CaretRange [pv|1.2.3|])

      it "parses tilde comparators" $ do
        strictParseSimple "~1.2.3" `shouldBe` Right (TildeRange [pv|1.2.3|])

      it "parses simple comparators with trailing content" $ do
        parseSimple "* 1.2.3" `shouldBe` Right (Primitive (Comparator Equal [pv|*|]))
        parseSimple "<1.2.3 || 5" `shouldBe` Right (Primitive (Comparator LessThan [pv|1.2.3|]))

      it "rejects invalid formats" $ do
        isLeft (strictParseSimple "") `shouldBe` True
        isLeft (strictParseSimple "foo") `shouldBe` True
        isLeft (strictParseSimple "$1.2.3") `shouldBe` True

    describe "versionBounds" $ do
      let simple ~> expectedInterval =
            it (show simple) $ versionBounds simple `shouldBe` expectedInterval

      -- Prmitive range bounds already tested in Comparator test

      -- Tilde range bounds
      TildeRange [pv|1.2.3|] ~> [vi| [1.2.3, 1.3.0) |]
      TildeRange [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      TildeRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      TildeRange [pv|*|] ~> allVersionsInterval

      -- Caret range bounds
      CaretRange [pv|1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
      CaretRange [pv|0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
      CaretRange [pv|0.0.3|] ~> [vi| [0.0.3, 0.0.4) |]
      CaretRange [pv|1.2|] ~> [vi| [1.2.0, 2.0.0) |]
      CaretRange [pv|0.2|] ~> [vi| [0.2.0, 0.3.0) |]
      CaretRange [pv|0.0|] ~> [vi| [0.0.0, 0.1.0) |]
      CaretRange [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      CaretRange [pv|0|] ~> [vi| [0.0.0, 1.0.0) |]
      CaretRange [pv|*|] ~> allVersionsInterval

  describe "Comparator" $ do
    it "show" $ do
      show (Comparator GreaterThanOrEqual [pv|1.2.3|]) `shouldBe` ">=1.2.3"
      show (Comparator GreaterThan [pv|1.2.3|]) `shouldBe` ">1.2.3"
      show (Comparator LessThanOrEqual [pv|1.2.3|]) `shouldBe` "<=1.2.3"
      show (Comparator LessThan [pv|1.2.3|]) `shouldBe` "<1.2.3"
      show (Comparator Equal [pv|1.2.3|]) `shouldBe` "1.2.3"

    describe "comparatorParser" $ do
      let parseComp = P.parse comparatorParser ""
          strictParseComp = P.parse (comparatorParser <* P.eof) ""

      it "parses comparators" $ do
        strictParseComp "=1.2.3" `shouldBe` Right (Comparator Equal [pv|1.2.3|])
        strictParseComp "=  1.2.3" `shouldBe` Right (Comparator Equal [pv|1.2.3|])
        strictParseComp "1.2.3" `shouldBe` Right (Comparator Equal [pv|1.2.3|])
        strictParseComp "    1.2.3" `shouldBe` Right (Comparator Equal [pv|1.2.3|])
        strictParseComp ">1.2.3" `shouldBe` Right (Comparator GreaterThan [pv|1.2.3|])
        strictParseComp ">  1.2.3" `shouldBe` Right (Comparator GreaterThan [pv|1.2.3|])
        strictParseComp "<1.2.3" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])
        strictParseComp "<    1.2.3" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])
        strictParseComp ">=1.2.3" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1.2.3|])
        strictParseComp ">=  1.2.3" `shouldBe` Right (Comparator GreaterThanOrEqual [pv|1.2.3|])
        strictParseComp "<=1.2.3" `shouldBe` Right (Comparator LessThanOrEqual [pv|1.2.3|])
        strictParseComp "<= 1.2.3" `shouldBe` Right (Comparator LessThanOrEqual [pv|1.2.3|])

      it "parses comparators with trailing content" $ do
        parseComp "<1.2.3 || 5" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])
        parseComp "<1.2.3 a 5" `shouldBe` Right (Comparator LessThan [pv|1.2.3|])

      it "rejects invalid formats" $ do
        isLeft (strictParseComp "") `shouldBe` True
        isLeft (strictParseComp "foo") `shouldBe` True
        isLeft (strictParseComp "$1.2.3") `shouldBe` True
        isLeft (strictParseComp "?1.x.x") `shouldBe` True

    describe "versionBounds" $ do
      let comparator ~> expectedInterval =
            it (show comparator) $ versionBounds comparator `shouldBe` expectedInterval

      -- Equal
      Comparator Equal [pv|1.2.3|] ~> [vi| [1.2.3, 1.2.3] |]
      Comparator Equal [pv|1.2|] ~> [vi| [1.2.0, 1.3.0) |]
      Comparator Equal [pv|1|] ~> [vi| [1.0.0, 2.0.0) |]
      Comparator Equal [pv|*|] ~> allVersionsInterval

      -- GreaterThan
      Comparator GreaterThan [pv|1.2.3|] ~> [vi| (1.2.3, inf) |]
      Comparator GreaterThan [pv|1.2|] ~> [vi| [1.3.0, inf) |]
      Comparator GreaterThan [pv|1|] ~> [vi| [2.0.0, inf) |]
      Comparator GreaterThan [pv|*|] ~> noVersionInterval

      -- LessThan
      Comparator LessThan [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3) |]
      Comparator LessThan [pv|1.2|] ~> [vi| [0.0.0, 1.2.0) |]
      Comparator LessThan [pv|1|] ~> [vi| [0.0.0, 1.0.0) |]
      Comparator LessThan [pv|*|] ~> noVersionInterval

      -- GreaterThanOrEqual
      Comparator GreaterThanOrEqual [pv|1.2.3|] ~> [vi| [1.2.3, inf) |]
      Comparator GreaterThanOrEqual [pv|1.2|] ~> [vi| [1.2.0, inf) |]
      Comparator GreaterThanOrEqual [pv|1|] ~> [vi| [1.0.0, inf) |]
      Comparator GreaterThanOrEqual [pv|*|] ~> allVersionsInterval

      -- LessThanOrEqual
      Comparator LessThanOrEqual [pv|1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
      Comparator LessThanOrEqual [pv|1.2|] ~> [vi| [0.0.0, 1.3.0) |]
      Comparator LessThanOrEqual [pv|1|] ~> [vi| [0.0.0, 2.0.0) |]
      Comparator LessThanOrEqual [pv|*|] ~> allVersionsInterval
