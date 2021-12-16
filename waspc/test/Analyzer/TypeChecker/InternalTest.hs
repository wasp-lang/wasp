module Analyzer.TypeChecker.InternalTest where

import Analyzer.TestUtil (ctx, fromWithCtx, wctx)
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

inferExprType' :: Bindings -> P.WithCtx P.Expr -> Either TypeError (WithCtx TypedExpr)
inferExprType' bindings expr = runWithBound bindings TD.empty $ inferExprType expr

test :: String -> P.WithCtx P.Expr -> Either TypeError Type -> SpecWith (Arg Expectation)
test name expr expected = it name $ do
  let actual = exprType . fromWithCtx <$> inferExprType' H.empty expr
  actual `shouldBe` expected

testSuccess :: String -> P.WithCtx P.Expr -> Type -> SpecWith (Arg Expectation)
testSuccess name expr = test name expr . Right

testFail :: String -> P.WithCtx P.Expr -> TypeError -> SpecWith (Arg Expectation)
testFail name expr = test name expr . Left

spec_Internal :: Spec
spec_Internal = do
  describe "Analyzer.TypeChecker.Internal" $ do
    describe "unify" $ do
      let ctx1 = ctx 1 1
          ctx2 = ctx 1 10
          ctx3 = ctx 2 5
          ctx4 = ctx 2 12
          ctx5 = ctx 2 20
          ctx6 = ctx 3 3
          ctx7 = ctx 3 11
          wctx2 = WithCtx ctx2
          wctx3 = WithCtx ctx3
          wctx4 = WithCtx ctx4
          wctx5 = WithCtx ctx5
          wctx6 = WithCtx ctx6
          wctx7 = WithCtx ctx7

      it "Doesn't affect 2 expressions of the same type" $ do
        property $ \(a, b) ->
          let initial = wctx2 (IntegerLiteral a) :| [wctx3 $ DoubleLiteral b]
              actual = unify ctx1 initial
           in actual == Right (initial, NumberType)
      it "Unifies two same-typed dictionaries to their original type" $ do
        let typ = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
        let a = wctx2 $ Dict [("a", wctx3 $ BoolLiteral True), ("b", wctx4 $ IntegerLiteral 2)] typ
        let b = wctx5 $ Dict [("a", wctx6 $ BoolLiteral True), ("b", wctx7 $ DoubleLiteral 3.14)] typ
        let texprs = a :| [b]
        unify ctx1 texprs
          `shouldBe` Right (texprs, typ)
      it "Unifies an empty dict and a dict with one property" $ do
        let a = wctx2 $ Dict [] (DictType H.empty)
        let b = wctx3 $ Dict [("a", wctx4 $ BoolLiteral True)] (DictType $ H.singleton "a" $ DictRequired BoolType)
        let expectedType = DictType $ H.singleton "a" $ DictOptional BoolType
        fmap (fmap (exprType . fromWithCtx) . fst) (unify ctx1 (a :| [b]))
          `shouldBe` Right (expectedType :| [expectedType])
      it "Is idempotent when unifying an empty dict and a singleton dict" $ do
        let a = wctx2 $ Dict [] (DictType H.empty)
        let b = wctx3 $ Dict [("a", wctx4 $ BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        unify ctx1 (a :| [b]) `shouldBe` (unify ctx1 (a :| [b]) >>= unify ctx1 . fst)
      it "Unifies an empty list with any other list" $ do
        let a = wctx2 $ List [] EmptyListType
        let b = wctx3 $ List [wctx4 $ StringLiteral "a"] (ListType StringType)
        let expected = ListType StringType
        fmap (fmap (exprType . fromWithCtx) . fst) (unify ctx1 (a :| [b]))
          `shouldBe` Right (expected :| [expected])

    describe "inferExprType" $ do
      testSuccess "Types string literals as StringType" (wctx 1 1 $ P.StringLiteral "string") StringType
      testSuccess "Types integer literals as NumberType" (wctx 1 1 $ P.IntegerLiteral 42) NumberType
      testSuccess "Types double literals as NumberType" (wctx 1 1 $ P.DoubleLiteral 3.14) NumberType
      testSuccess "Types bool literals as BoolType" (wctx 1 1 $ P.BoolLiteral True) BoolType
      testSuccess
        "Types external imports as ExtImportType"
        (wctx 1 1 $ P.ExtImport (P.ExtImportModule "Main") "main.js")
        ExtImportType

      testSuccess "Types quoted json as JSONType" (wctx 1 1 $ P.Quoter "json" "\"key\": \"value\"") (QuoterType "json")
      testSuccess "Types quoted psl as PSLType" (wctx 1 1 $ P.Quoter "psl" "id Int @id") (QuoterType "psl")
      testFail
        "Fails to type check quoters with tag besides json or psl"
        (wctx 1 1 $ P.Quoter "toml" "key = \"value\"")
        (mkTypeError (ctx 1 1) $ QuoterUnknownTag "toml")

      it "Types identifier as the type in the bindings" $ do
        forAll chooseType $ \typ ->
          let bindings = H.singleton "var" typ
              actual = exprType . fromWithCtx <$> inferExprType' bindings (wctx 1 1 $ P.Var "var")
           in actual == Right typ
      it "Fails to type check identifiers not given a type in the bindings" $ do
        let bindings = H.empty
        let actual = exprType . fromWithCtx <$> inferExprType' bindings (wctx 1 1 $ P.Var "pi")
        let expected = Left $ mkTypeError (ctx 1 1) $ UndefinedIdentifier "pi"
        actual `shouldBe` expected

      testSuccess
        "Type checks a dictionary"
        (wctx 1 1 $ P.Dict [("a", wctx 2 5 $ P.IntegerLiteral 5), ("b", wctx 3 5 $ P.StringLiteral "string")])
        (DictType $ H.fromList [("a", DictRequired NumberType), ("b", DictRequired StringType)])
      testFail
        "Fails to type check a dictionary with duplicated keys"
        (wctx 1 1 $ P.Dict [("a", wctx 2 5 $ P.IntegerLiteral 5), ("a", wctx 3 5 $ P.IntegerLiteral 6)])
        (mkTypeError (ctx 1 1) $ DictDuplicateField "a")

      testSuccess
        "Type checks an empty list as EmptyListType"
        (wctx 1 1 $ P.List [])
        EmptyListType
      testSuccess
        "Type checks a list where all elements have the same type"
        (wctx 1 1 $ P.List [wctx 1 10 $ P.IntegerLiteral 5, wctx 1 20 $ P.DoubleLiteral 1.6])
        (ListType NumberType)
      testFail
        "Fails to type check a list containing strings and numbers"
        (wctx 1 1 $ P.List [wctx 1 10 $ P.IntegerLiteral 5, wctx 1 20 $ P.StringLiteral "4"])
        ( mkTypeError (ctx 1 1) $
            UnificationError $
              TypeCoercionError (wctx 1 20 $ StringLiteral "4") NumberType ReasonUncoercable
        )

      testSuccess
        "Type checks a list of dictionaries that unify but have different types"
        ( wctx 1 1 $
            P.List
              [ wctx 2 2 $ P.Dict [("a", wctx 2 10 $ P.IntegerLiteral 5)],
                wctx 3 2 $ P.Dict [],
                wctx 4 2 $ P.Dict [("b", wctx 4 10 $ P.StringLiteral "string")]
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
        ( wctx 1 1 $
            P.List
              [ wctx 2 2 $ P.Dict [("a", wctx 2 10 $ P.IntegerLiteral 5)],
                wctx 3 2 $ P.Dict [("a", wctx 3 10 $ P.StringLiteral "string")]
              ]
        )
        ( mkTypeError (ctx 1 1) $
            UnificationError $
              TypeCoercionError
                ( wctx 3 2 $
                    Dict
                      [("a", wctx 3 10 $ StringLiteral "string")]
                      (DictType $ H.singleton "a" (DictRequired StringType))
                )
                (DictType $ H.singleton "a" (DictRequired NumberType))
                ( ReasonDictWrongKeyType
                    "a"
                    ( TypeCoercionError
                        (wctx 3 10 $ StringLiteral "string")
                        NumberType
                        ReasonUncoercable
                    )
                )
        )

      describe "Type checks a tuple" $ do
        testSuccess
          "When tuple is a pair"
          ( wctx 1 1 $
              P.Tuple
                ( wctx 2 2 $ P.IntegerLiteral 5,
                  wctx 2 10 $ P.StringLiteral "string",
                  []
                )
          )
          (TupleType (NumberType, StringType, []))
        testSuccess
          "When tuple is a triple"
          ( wctx 1 1 $
              P.Tuple
                ( wctx 2 2 $ P.IntegerLiteral 5,
                  wctx 2 10 $ P.StringLiteral "string",
                  [wctx 2 20 $ P.IntegerLiteral 2]
                )
          )
          (TupleType (NumberType, StringType, [NumberType]))

    describe "checkStmt" $ do
      it "Type checks existing declaration type with correct argument" $ do
        let ast = wctx 1 1 $ P.Decl "string" "App" $ wctx 2 3 $ P.StringLiteral "Wasp"
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
        let expected = Right $ wctx 1 1 $ Decl "App" (wctx 2 3 $ StringLiteral "Wasp") (DeclType "string")
        actual `shouldBe` expected
      it "Fails to type check non-existant declaration type" $ do
        let ast = wctx 1 1 $ P.Decl "string" "App" $ wctx 2 3 $ P.StringLiteral "Wasp"
        let actual = run TD.empty $ checkStmt ast
        actual `shouldBe` Left (mkTypeError (ctx 1 1) $ NoDeclarationType "string")
      it "Fails to type check existing declaration type with incorrect argument" $ do
        let ast = wctx 1 1 $ P.Decl "string" "App" $ wctx 2 3 $ P.IntegerLiteral 5
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
        let expectedError =
              mkTypeError (ctx 1 1) $
                WeakenError $
                  TypeCoercionError
                    (wctx 2 3 $ IntegerLiteral 5)
                    StringType
                    ReasonUncoercable
        actual `shouldBe` Left expectedError
      it "Type checks declaration with dict type with an argument that unifies to the correct type" $ do
        let ast = wctx 1 1 $ P.Decl "maybeString" "App" $ wctx 2 3 $ P.Dict [("val", wctx 2 10 $ P.StringLiteral "Wasp")]
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
                wctx 1 1 $
                  Decl
                    "App"
                    ( wctx 2 3 $
                        Dict
                          [("val", wctx 2 10 $ StringLiteral "Wasp")]
                          (DictType $ H.singleton "val" (DictOptional StringType))
                    )
                    (DeclType "maybeString")
        actual `shouldBe` expected
