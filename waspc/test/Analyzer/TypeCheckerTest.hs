module Analyzer.TypeCheckerTest where

import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker
import Analyzer.TypeChecker.Internal (Bindings, runTWithBound)
import qualified Data.HashMap.Strict as H
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

chooseType :: Gen Type
chooseType =
  elements
    [ StringType,
      NumberType,
      BoolType,
      ExtImportType,
      QuoterType "json",
      QuoterType "psl"
    ]

checkExpr' :: Bindings -> P.Expr -> Either TypeError TypedExpr
checkExpr' bindings expr = runTWithBound bindings $ checkExpr expr

spec_Parser :: Spec
spec_Parser = do
  describe "Analyzer.TypeChecker" $ do
    describe "checkExpr" $ do
      it "Types string literals as StringType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.StringLiteral "string")
        let expected = Right StringType
        actual `shouldBe` expected
      it "Types integer literals as NumberType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.IntegerLiteral 42)
        let expected = Right NumberType
        actual `shouldBe` expected
      it "Types double literals as NumberType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.DoubleLiteral 3.14)
        let expected = Right NumberType
        actual `shouldBe` expected
      it "Types bool literals as BoolType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.BoolLiteral True)
        let expected = Right BoolType
        actual `shouldBe` expected
      it "Types external imports as ExtImportType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.ExtImport (P.ExtImportModule "Main") "main.js")
        let expected = Right ExtImportType
        actual `shouldBe` expected

      it "Types quoted json as JSONType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.Quoter "json" "\"field\": \"value\"")
        let expected = Right $ QuoterType "json"
        actual `shouldBe` expected
      it "Types quoted psl as PSLType" $ do
        let actual = exprType <$> checkExpr' H.empty (P.Quoter "psl" "id Int @id")
        let expected = Right $ QuoterType "psl"
        actual `shouldBe` expected
      it "Fails to type check quoters with tag besides json or psl" $ do
        let actual = checkExpr' H.empty (P.Quoter "toml" "field = \"value\"")
        let expected = Left $ TypeError "Unknown Quoter tag 'toml'"
        actual `shouldBe` expected

      it "Types identifier as the type in the bindings" $ do
        forAll chooseType $ \typ ->
          let bindings = H.singleton "var" typ
              actual = exprType <$> checkExpr' bindings (P.Identifier "var")
           in actual == Right typ
      it "Fails to type check identifiers not given a type in the bindings" $ do
        let bindings = H.empty
        let actual = exprType <$> checkExpr' bindings (P.Identifier "pi")
        let expected = Left $ TypeError "Undefined identifier 'pi'"
        actual `shouldBe` expected
