module Analyzer.TypeChecker.InternalTest where

import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker.AST
import Analyzer.TypeChecker.Internal
import Analyzer.TypeChecker.Monad (Bindings, run, runWithBound)
import Analyzer.TypeChecker.TypeError
import qualified Analyzer.TypeDefinitions as TD
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

-- TODO:
-- Having Arbitrary instances for Types and TypedAST would allow much more thorough testing
-- of the type checker, and is probably worth the effort to figure out how to make instances
-- given how complex the code is, how many corner cases there are, and how well-defined the
-- required properties of the type-checking functions are.

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
checkExpr' bindings expr = runWithBound bindings TD.empty $ checkExpr expr

spec_Parser :: Spec
spec_Parser = do
  describe "Analyzer.TypeChecker.Internal" $ do
    describe "unify" $ do
      it "Doesn't affect 2 expressions of the same type" $ do
        property $ \(a, b) ->
          let initial = IntegerLiteral a :| [DoubleLiteral b]
              actual = unify initial
           in actual == Right (initial, NumberType)
      it "Unifies two same-typed dictionaries to their original type" $ do
        let typ = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
        let a = Dict [("a", BoolLiteral True), ("b", IntegerLiteral 2)] typ
        let b = Dict [("a", BoolLiteral True), ("b", DoubleLiteral 3.14)] typ
        unify (a :| [b])
          `shouldBe` Right (a :| [b], typ)
      it "Unifies an empty dict and a dict with one property" $ do
        let a = Dict [] (DictType H.empty)
        let b = Dict [("a", BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        let expected = DictType $ H.singleton "a" $ DictOptional BoolType
        fmap (fmap exprType . fst) (unify (a :| [b]))
          `shouldBe` Right (expected :| [expected])
      it "Is idempotent when unifying an empty dict and a singleton dict" $ do
        let a = Dict [] (DictType H.empty)
        let b = Dict [("a", BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        unify (a :| [b]) `shouldBe` (unify (a :| [b]) >>= unify . fst)

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
        let expected = Left $ QuoterUnknownTag "toml"
        actual `shouldBe` expected

      it "Types identifier as the type in the bindings" $ do
        forAll chooseType $ \typ ->
          let bindings = H.singleton "var" typ
              actual = exprType <$> checkExpr' bindings (P.Identifier "var")
           in actual == Right typ
      it "Fails to type check identifiers not given a type in the bindings" $ do
        let bindings = H.empty
        let actual = exprType <$> checkExpr' bindings (P.Identifier "pi")
        let expected = Left $ UndefinedIdentifier "pi"
        actual `shouldBe` expected

      it "Type checks a dictionary" $ do
        let ast = P.Dict [("a", P.IntegerLiteral 5), ("b", P.StringLiteral "string")]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected = Right $ DictType $ H.fromList [("a", DictRequired NumberType), ("b", DictRequired StringType)]
        actual `shouldBe` expected
      it "Fails to type check a dictionary with duplicated keys" $ do
        let ast = P.Dict [("a", P.IntegerLiteral 5), ("a", P.IntegerLiteral 6)]
        let actual = exprType <$> checkExpr' H.empty ast
        actual `shouldBe` Left (DictDuplicateField "a")

      it "Fails to type check an empty list" $ do
        -- TODO: this test must be removed/changed when empty lists are implemented.
        (exprType <$> checkExpr' H.empty (P.List [])) `shouldBe` Left EmptyListNotImplemented
      it "Type checks a list where all elements have the same type" $ do
        let ast = P.List [P.IntegerLiteral 5, P.DoubleLiteral 1.6]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected = Right $ ListType NumberType
        actual `shouldBe` expected
      it "Fails to type check a list containing strings and numbers" $ do
        let ast = P.List [P.IntegerLiteral 5, P.StringLiteral "4"]
        let actual = exprType <$> checkExpr' H.empty ast
        let expected = Left $ UnificationError ReasonUncoercable NumberType StringType
        actual `shouldBe` expected
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
        let expectedError =
              UnificationError
                (ReasonDictWrongKeyType "a" (UnificationError ReasonUncoercable NumberType StringType))
                (DictType $ H.singleton "a" (DictRequired NumberType))
                (DictType $ H.singleton "a" (DictRequired StringType))
        actual `shouldBe` Left expectedError

    describe "checkStmt" $ do
      it "Type checks existing declaration type with correct argument" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let lib = TD.TypeDefinitions {TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType), TD.enumTypes = H.empty}
        let actual = run lib $ checkStmt ast
        let expected = Right $ Decl "App" (StringLiteral "Wasp") (DeclType "string")
        actual `shouldBe` expected
      it "Fails to type check non-existant declaration type" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let actual = run TD.empty $ checkStmt ast
        actual `shouldBe` Left (NoDeclarationType "string")
      it "Fails to type check existing declaration type with incorrect argument" $ do
        let ast = P.Decl "string" "App" (P.IntegerLiteral 5)
        let lib =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType),
                  TD.enumTypes = H.empty
                }
        let actual = run lib $ checkStmt ast
        let expectedError = WeakenError ReasonUncoercable (IntegerLiteral 5) StringType
        actual `shouldBe` Left expectedError
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
        let actual = run lib $ checkStmt ast
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
