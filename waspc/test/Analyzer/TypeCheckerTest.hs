module Analyzer.TypeCheckerTest where

import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker
import Analyzer.TypeChecker.Internal (Bindings, runT, runTWithBound)
import qualified Analyzer.TypeDefinitions as TD
import Data.Either (isLeft, isRight)
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
checkExpr' bindings expr = runTWithBound bindings TD.empty $ checkExpr expr

spec_Parser :: Spec
spec_Parser = do
  describe "Analyzer.TypeChecker" $ do
    describe "unify" $ do
      it "Doesn't affect 2 expressions of the same type" $ do
        property $ \(a, b) ->
          let initial = [IntegerLiteral a, DoubleLiteral b]
              actual = unify initial
           in actual == Right initial
      it "Unifies two same-typed dictionaries to their original type" $ do
        let typ = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
        let a = Dict [("a", BoolLiteral True), ("b", IntegerLiteral 2)] typ
        let b = Dict [("a", BoolLiteral True), ("b", DoubleLiteral 3.14)] typ
        unify [a, b]
          `shouldBe` Right [a, b]
      it "Unifies an empty dict and a dict with one property" $ do
        let a = Dict [] (DictType H.empty)
        let b = Dict [("a", BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        let expected = DictType $ H.singleton "a" $ DictOptional BoolType
        fmap (map exprType) (unify [a, b])
          `shouldBe` Right [expected, expected]
      it "Is idempotent when unifying an empty dict and a singleton dict" $ do
        let a = Dict [] (DictType H.empty)
        let b = Dict [("a", BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        unify [a, b] `shouldBe` (unify [a, b] >>= unify)

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

      it "Type checks a dictionary" $ do
        let ast = P.Dict [("a", P.IntegerLiteral 5), ("b", P.StringLiteral "string")]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected = Right $ DictType $ H.fromList [("a", DictRequired NumberType), ("b", DictRequired StringType)]
        actual `shouldBe` expected
      it "Fails to type check a dictionary with duplicated keys" $ do
        let ast = P.Dict [("a", P.IntegerLiteral 5), ("a", P.IntegerLiteral 6)]
        let actual = exprType <$> checkExpr' H.empty ast
        actual `shouldSatisfy` isLeft

      it "Type checks an empty list" $ do
        (exprType <$> checkExpr' H.empty (P.List [])) `shouldSatisfy` isRight
      it "Type checks a list where all elements have the same type" $ do
        let ast = P.List [P.IntegerLiteral 5, P.DoubleLiteral 1.6]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected = Right $ ListType NumberType
        actual `shouldBe` expected
      it "Fails to type check a list containing strings and numbers" $ do
        let ast = P.List [P.IntegerLiteral 5, P.StringLiteral "4"]
        let actual = exprType <$> checkExpr' H.empty ast
        actual `shouldSatisfy` isLeft
      it "Type checks a list of dictionaries that unify but have different types" $ do
        let ast =
              P.List
                [ P.Dict [("a", P.IntegerLiteral 5)],
                  P.Dict [],
                  P.Dict [("b", P.StringLiteral "string")]
                ]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected =
              Right $
                ListType $
                  DictType $
                    H.fromList
                      [ ("a", DictOptional NumberType),
                        ("b", DictOptional StringType)
                      ]
        actual `shouldBe` expected
      it "Fails to type check a list of dictionaries that do not unify" $ do
        let ast =
              P.List
                [ P.Dict [("a", P.IntegerLiteral 5)],
                  P.Dict [("a", P.StringLiteral "string")]
                ]
        let actual = exprType <$> checkExpr' H.empty ast
        actual `shouldSatisfy` isLeft

    describe "checkStmt" $ do
      it "Type checks existing declaration type with correct argument" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let lib = TD.TypeDefinitions {TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType), TD.enumTypes = H.empty}
        let actual = runT lib $ checkStmt ast
        let expected = Right $ Decl "App" (StringLiteral "Wasp") (DeclType "string")
        actual `shouldBe` expected
      it "Fails to type check non-existant declaration type" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let actual = runT TD.empty $ checkStmt ast
        actual `shouldSatisfy` isLeft
      it "Fails to type check existing declaration type with incorrect argument" $ do
        let ast = P.Decl "string" "App" (P.IntegerLiteral 5)
        let lib =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType),
                  TD.enumTypes = H.empty
                }
        let actual = runT lib $ checkStmt ast
        actual `shouldSatisfy` isLeft
      it "Type checks declaration with dict type with an argument that unifies to the correct type" $ do
        let ast = P.Decl "maybeString" "App" (P.Dict [("val", P.StringLiteral "Wasp")])
        let lib =
              TD.TypeDefinitions
                { TD.declTypes =
                    H.singleton "maybeString" $
                      TD.DeclType "maybeString" $
                        DictType $ H.singleton "val" (DictOptional StringType),
                  TD.enumTypes = H.empty
                }
        let actual = runT lib $ checkStmt ast
        let expected =
              Right $
                Decl
                  "App"
                  ( Dict
                      [("val", StringLiteral "Wasp")]
                      (DictType $ H.singleton "val" (DictOptional StringType))
                  )
                  (DeclType "maybeString")
        actual `shouldBe` expected

    describe "typeCheck" $ do
      it "Type checks a simple, well-typed example" $ do
        let ast =
              P.AST
                [ P.Decl "app" "Todo" (P.Dict [("title", P.StringLiteral "Todo App")]),
                  P.Decl "app" "Trello" (P.Dict [("title", P.StringLiteral "Trello Clone")])
                ]
        let lib =
              TD.TypeDefinitions
                { TD.declTypes =
                    H.fromList
                      [ ( "app",
                          TD.DeclType "app" $
                            DictType $
                              H.fromList
                                [ ("title", DictOptional StringType)
                                ]
                        )
                      ],
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck lib ast
        actual `shouldSatisfy` isRight
      it "Fails to type check a simple, ill-typed example" $ do
        let ast = P.AST [P.Decl "string" "App" (P.IntegerLiteral 5)]
        let lib =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck lib ast
        actual `shouldSatisfy` isLeft
      it "Type checks an existing enum value" $ do
        let ast = P.AST [P.Decl "food" "Cucumber" (P.Identifier "Dill")]
        let lib =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "food" (TD.DeclType "food" (EnumType "flavor")),
                  TD.enumTypes = H.singleton "flavor" (TD.EnumType "flavor" ["Fresh", "Dill"])
                }
        let actual = typeCheck lib ast
        let expected = Right $ TypedAST [Decl "Cucumber" (Var "Dill" (EnumType "flavor")) (DeclType "food")]
        actual `shouldBe` expected
