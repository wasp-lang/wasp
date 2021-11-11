module Analyzer.TypeChecker.InternalTest where

import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.AST
import Wasp.Analyzer.TypeChecker.Internal
import Wasp.Analyzer.TypeChecker.Monad (Bindings, run, runWithBound)
import Wasp.Analyzer.TypeChecker.TypeError
import qualified Wasp.Analyzer.TypeDefinitions as TD
import qualified Wasp.Analyzer.TypeDefinitions.Internal as TD

-- TODO:
-- Having Arbitrary instances for Types and TypedAST would allow much more thorough testing
-- of the type checker, and is probably worth the effort to figure out how to make instances
-- given how complex the code is, how many corner cases there are, and how well-defined the
-- required properties of the type-checking functions are.

-- TODO:
-- Use the parser to generate AST instead of writing it by hand. Consider using quasiquotes
-- instead of calling "parse" directly.

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

inferExprType' :: Bindings -> P.Expr -> Either TypeError TypedExpr
inferExprType' bindings expr = runWithBound bindings TD.empty $ inferExprType expr

test :: String -> P.Expr -> Either TypeError Type -> SpecWith (Arg Expectation)
test name expr expected = it name $ do
  let actual = exprType <$> inferExprType' H.empty expr
  actual `shouldBe` expected

testSuccess :: String -> P.Expr -> Type -> SpecWith (Arg Expectation)
testSuccess name expr = test name expr . Right

testFail :: String -> P.Expr -> TypeError -> SpecWith (Arg Expectation)
testFail name expr = test name expr . Left

spec_Internal :: Spec
spec_Internal = do
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
      it "Unifies an empty list with any other list" $ do
        let a = List [] EmptyListType
        let b = List [StringLiteral "a"] (ListType StringType)
        let expected = ListType StringType
        fmap (fmap exprType . fst) (unify (a :| [b]))
          `shouldBe` Right (expected :| [expected])

    describe "inferExprType" $ do
      testSuccess "Types string literals as StringType" (P.StringLiteral "string") StringType
      testSuccess "Types integer literals as NumberType" (P.IntegerLiteral 42) NumberType
      testSuccess "Types double literals as NumberType" (P.DoubleLiteral 3.14) NumberType
      testSuccess "Types bool literals as BoolType" (P.BoolLiteral True) BoolType
      testSuccess
        "Types external imports as ExtImportType"
        (P.ExtImport (P.ExtImportModule "Main") "main.js")
        ExtImportType

      testSuccess "Types quoted json as JSONType" (P.Quoter "json" "\"key\": \"value\"") (QuoterType "json")
      testSuccess "Types quoted psl as PSLType" (P.Quoter "psl" "id Int @id") (QuoterType "psl")
      testFail
        "Fails to type check quoters with tag besides json or psl"
        (P.Quoter "toml" "key = \"value\"")
        (QuoterUnknownTag "toml")

      it "Types identifier as the type in the bindings" $ do
        forAll chooseType $ \typ ->
          let bindings = H.singleton "var" typ
              actual = exprType <$> inferExprType' bindings (P.Var "var")
           in actual == Right typ
      it "Fails to type check identifiers not given a type in the bindings" $ do
        let bindings = H.empty
        let actual = exprType <$> inferExprType' bindings (P.Var "pi")
        let expected = Left $ UndefinedIdentifier "pi"
        actual `shouldBe` expected

      testSuccess
        "Type checks a dictionary"
        (P.Dict [("a", P.IntegerLiteral 5), ("b", P.StringLiteral "string")])
        (DictType $ H.fromList [("a", DictRequired NumberType), ("b", DictRequired StringType)])
      testFail
        "Fails to type check a dictionary with duplicated keys"
        (P.Dict [("a", P.IntegerLiteral 5), ("a", P.IntegerLiteral 6)])
        (DictDuplicateField "a")

      testSuccess
        "Type checks an empty list as EmptyListType"
        (P.List [])
        EmptyListType
      testSuccess
        "Type checks a list where all elements have the same type"
        (P.List [P.IntegerLiteral 5, P.DoubleLiteral 1.6])
        (ListType NumberType)
      testFail
        "Fails to type check a list containing strings and numbers"
        (P.List [P.IntegerLiteral 5, P.StringLiteral "4"])
        (UnificationError ReasonUncoercable NumberType StringType)

      testSuccess
        "Type checks a list of dictionaries that unify but have different types"
        ( P.List
            [ P.Dict [("a", P.IntegerLiteral 5)],
              P.Dict [],
              P.Dict [("b", P.StringLiteral "string")]
            ]
        )
        ( ListType $
            DictType $
              H.fromList
                [ ("a", DictOptional NumberType),
                  ("b", DictOptional StringType)
                ]
        )
      testFail
        "Fails to type check a list of dictionaries that do not unify"
        ( P.List
            [ P.Dict [("a", P.IntegerLiteral 5)],
              P.Dict [("a", P.StringLiteral "string")]
            ]
        )
        ( UnificationError
            (ReasonDictWrongKeyType "a" (UnificationError ReasonUncoercable NumberType StringType))
            (DictType $ H.singleton "a" (DictRequired NumberType))
            (DictType $ H.singleton "a" (DictRequired StringType))
        )

    describe "checkStmt" $ do
      it "Type checks existing declaration type with correct argument" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let typeDefs = TD.TypeDefinitions {TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined), TD.enumTypes = H.empty}
        let actual = run typeDefs $ checkStmt ast
        let expected = Right $ Decl "App" (StringLiteral "Wasp") (DeclType "string")
        actual `shouldBe` expected
      it "Fails to type check non-existant declaration type" $ do
        let ast = P.Decl "string" "App" (P.StringLiteral "Wasp")
        let actual = run TD.empty $ checkStmt ast
        actual `shouldBe` Left (NoDeclarationType "string")
      it "Fails to type check existing declaration type with incorrect argument" $ do
        let ast = P.Decl "string" "App" (P.IntegerLiteral 5)
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
        let expectedError = WeakenError ReasonUncoercable (IntegerLiteral 5) StringType
        actual `shouldBe` Left expectedError
      it "Type checks declaration with dict type with an argument that unifies to the correct type" $ do
        let ast = P.Decl "maybeString" "App" (P.Dict [("val", P.StringLiteral "Wasp")])
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes =
                    H.singleton "maybeString" $
                      TD.DeclType
                        "maybeString"
                        (DictType $ H.singleton "val" (DictOptional StringType))
                        undefined,
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
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
