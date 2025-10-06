module Analyzer.TypeChecker.InternalTest where

import Analyzer.TestUtil (ctx, fromWithCtx)
import Data.Either (isLeft)
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
      QuoterType "json"
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
    let ctx1 = ctx (1, 1) (1, 10)
        ctx2 = ctx (1, 10) (1, 15)
        ctx3 = ctx (2, 5) (3, 10)
        ctx4 = ctx (2, 12) (2, 20)
        ctx5 = ctx (2, 20) (2, 25)
        ctx6 = ctx (3, 3) (3, 18)
        ctx7 = ctx (3, 11) (3, 30)
        wctx1 = WithCtx ctx1
        wctx2 = WithCtx ctx2
        wctx3 = WithCtx ctx3
        wctx4 = WithCtx ctx4
        wctx5 = WithCtx ctx5
        wctx6 = WithCtx ctx6
        wctx7 = WithCtx ctx7

    describe "check" $ do
      describe "Correctly type checks an AST" $ do
        it "When a declaration is used before its definition" $ do
          let typeDefs =
                TD.TypeDefinitions
                  { TD.declTypes =
                      H.fromList
                        [ ("person", TD.DeclType "person" (DictType $ H.singleton "favoritePet" (DictRequired $ DeclType "pet")) undefined),
                          ("pet", TD.DeclType "pet" (DictType H.empty) undefined)
                        ],
                    TD.enumTypes = H.empty
                  }
          let ast =
                P.AST
                  [ wctx1 $ P.Decl "person" "John" $ wctx2 $ P.Dict [("favoritePet", wctx3 $ P.Var "Riu")],
                    wctx4 $ P.Decl "pet" "Riu" $ wctx5 $ P.Dict []
                  ]
          let actual = run typeDefs $ check ast
          let expected =
                Right $
                  TypedAST
                    [ wctx1 $
                        Decl
                          "John"
                          ( wctx2 $
                              Dict
                                [("favoritePet", wctx3 $ Var "Riu" (DeclType "pet"))]
                                (DictType $ H.singleton "favoritePet" (DictRequired $ DeclType "pet"))
                          )
                          (DeclType "person"),
                      wctx4 $
                        Decl
                          "Riu"
                          (wctx5 $ Dict [] (DictType H.empty))
                          (DeclType "pet")
                    ]

          actual `shouldBe` expected

    describe "checkStmt" $ do
      it "Type checks existing declaration type with correct argument" $ do
        let ast = wctx1 $ P.Decl "string" "App" $ wctx2 $ P.StringLiteral "Wasp"
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
        let expected = Right $ wctx1 $ Decl "App" (wctx2 $ StringLiteral "Wasp") (DeclType "string")
        actual `shouldBe` expected
      it "Fails to type check non-existant declaration type" $ do
        let ast = wctx1 $ P.Decl "string" "App" $ wctx2 $ P.StringLiteral "Wasp"
        let actual = run TD.empty $ checkStmt ast
        actual `shouldBe` Left (mkTypeError ctx1 $ NoDeclarationType "string")
      it "Fails to type check existing declaration type with incorrect argument" $ do
        let ast = wctx1 $ P.Decl "string" "App" $ wctx2 $ P.IntegerLiteral 5
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = run typeDefs $ checkStmt ast
        let expectedError =
              mkTypeError ctx1 $
                CoercionError $
                  TypeCoercionError
                    (wctx2 $ IntegerLiteral 5)
                    StringType
                    ReasonUncoercable
        actual `shouldBe` Left expectedError

      describe "A declaration statement with a body of type T satisfies a declaration type definition with a body of type S, when T is subtype of S." $ do
        it "When S is a dict with an optional field, and T is a dict with a required field" $ do
          let ast = wctx1 $ P.Decl "typeWithOptional" "Foo" $ wctx2 $ P.Dict [("val", wctx3 $ P.StringLiteral "Bar")]
          let typeDefs =
                TD.TypeDefinitions
                  { TD.declTypes =
                      H.singleton "typeWithOptional" $
                        TD.DeclType
                          "typeWithOptional"
                          (DictType $ H.singleton "val" (DictOptional StringType))
                          undefined,
                    TD.enumTypes = H.empty
                  }
          let actual = run typeDefs $ checkStmt ast
          let expected =
                Right $
                  wctx1 $
                    Decl
                      "Foo"
                      ( wctx2 $
                          Dict
                            [("val", wctx3 $ StringLiteral "Bar")]
                            (DictType $ H.singleton "val" (DictRequired StringType))
                      )
                      (DeclType "typeWithOptional")
          actual `shouldBe` expected

    describe "inferExprType" $ do
      testSuccess "Types string literals as StringType" (wctx1 $ P.StringLiteral "string") StringType
      testSuccess "Types integer literals as NumberType" (wctx1 $ P.IntegerLiteral 42) NumberType
      testSuccess "Types double literals as NumberType" (wctx1 $ P.DoubleLiteral 3.14) NumberType
      testSuccess "Types bool literals as BoolType" (wctx1 $ P.BoolLiteral True) BoolType
      testSuccess
        "Types external imports as ExtImportType"
        (wctx1 $ P.ExtImport (P.ExtImportModule "Main") "main.js")
        ExtImportType

      testSuccess "Types quoted json as JSONType" (wctx1 $ P.Quoter "json" "\"key\": \"value\"") (QuoterType "json")
      testFail
        "Fails to type check quoters with tag besides json or psl"
        (wctx1 $ P.Quoter "toml" "key = \"value\"")
        (mkTypeError ctx1 $ QuoterUnknownTag "toml")

      it "Types identifier as the type in the bindings" $ do
        forAll chooseType $ \typ ->
          let bindings = H.singleton "var" typ
              actual = exprType . fromWithCtx <$> inferExprType' bindings (wctx1 $ P.Var "var")
           in actual == Right typ
      it "Fails to type check identifiers not given a type in the bindings" $ do
        let bindings = H.empty
        let actual = exprType . fromWithCtx <$> inferExprType' bindings (wctx1 $ P.Var "pi")
        let expected = Left $ mkTypeError ctx1 $ UndefinedIdentifier "pi"
        actual `shouldBe` expected

      testSuccess
        "Type checks a dictionary"
        (wctx1 $ P.Dict [("a", wctx2 $ P.IntegerLiteral 5), ("b", wctx3 $ P.StringLiteral "string")])
        (DictType $ H.fromList [("a", DictRequired NumberType), ("b", DictRequired StringType)])
      testFail
        "Fails to type check a dictionary with duplicated keys"
        (wctx1 $ P.Dict [("a", wctx2 $ P.IntegerLiteral 5), ("a", wctx3 $ P.IntegerLiteral 6)])
        (mkTypeError ctx1 $ DictDuplicateField "a")

      testSuccess
        "Type checks an empty list as EmptyListType"
        (wctx1 $ P.List [])
        EmptyListType
      testSuccess
        "Type checks a list where all elements have the same type"
        (wctx1 $ P.List [wctx2 $ P.IntegerLiteral 5, wctx3 $ P.DoubleLiteral 1.6])
        (ListType NumberType)
      testFail
        "Fails to type check a list containing strings and numbers"
        (wctx1 $ P.List [wctx2 $ P.IntegerLiteral 5, wctx3 $ P.StringLiteral "4"])
        ( mkTypeError ctx1 $
            UnificationError $
              TypeCoercionError (wctx3 $ StringLiteral "4") NumberType ReasonUncoercable
        )

      testSuccess
        "Type checks a list of dictionaries that unify but have different types"
        ( wctx1 $
            P.List
              [ wctx2 $ P.Dict [("a", wctx3 $ P.IntegerLiteral 5)],
                wctx4 $ P.Dict [],
                wctx5 $ P.Dict [("b", wctx6 $ P.StringLiteral "string")]
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
        ( wctx1 $
            P.List
              [ wctx2 $ P.Dict [("a", wctx3 $ P.IntegerLiteral 5)],
                wctx4 $ P.Dict [("a", wctx5 $ P.StringLiteral "string")]
              ]
        )
        ( mkTypeError ctx1 $
            UnificationError $
              TypeCoercionError
                ( wctx4 $
                    Dict
                      [("a", wctx5 $ StringLiteral "string")]
                      (DictType $ H.singleton "a" (DictRequired StringType))
                )
                (DictType $ H.singleton "a" (DictRequired NumberType))
                ( ReasonDictWrongKeyType
                    "a"
                    ( TypeCoercionError
                        (wctx5 $ StringLiteral "string")
                        NumberType
                        ReasonUncoercable
                    )
                )
        )

      describe "Type checks a tuple" $ do
        testSuccess
          "When tuple is a pair"
          ( wctx1 $
              P.Tuple
                ( wctx2 $ P.IntegerLiteral 5,
                  wctx3 $ P.StringLiteral "string",
                  []
                )
          )
          (TupleType (NumberType, StringType, []))
        testSuccess
          "When tuple is a triple"
          ( wctx1 $
              P.Tuple
                ( wctx2 $ P.IntegerLiteral 5,
                  wctx3 $ P.StringLiteral "string",
                  [wctx4 $ P.IntegerLiteral 2]
                )
          )
          (TupleType (NumberType, StringType, [NumberType]))

    describe "unify" $ do
      it "Correctly unifies two expressions of the same type" $ do
        property $ \(a, b) ->
          let initial = wctx2 (IntegerLiteral a) :| [wctx3 $ DoubleLiteral b]
              actual = unify ctx1 initial
           in actual == Right NumberType
      it "Correctly unifies two dictionaries of the same type" $ do
        let typ = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
        let a = wctx2 $ Dict [("a", wctx3 $ BoolLiteral True), ("b", wctx4 $ IntegerLiteral 2)] typ
        let b = wctx5 $ Dict [("a", wctx6 $ BoolLiteral True), ("b", wctx7 $ DoubleLiteral 3.14)] typ
        let texprs = a :| [b]
        unify ctx1 texprs `shouldBe` Right typ
      it "Unifies an empty dict and a dict with one property" $ do
        let a = wctx2 $ Dict [] (DictType H.empty)
        let b = wctx3 $ Dict [("a", wctx4 $ BoolLiteral True)] (DictType $ H.singleton "a" $ DictRequired BoolType)
        let expectedType = DictType $ H.singleton "a" $ DictOptional BoolType
        unify ctx1 (a :| [b]) `shouldBe` Right expectedType
      it "Unifies an empty list with any other list" $ do
        let a = wctx2 $ List [] EmptyListType
        let b = wctx3 $ List [wctx4 $ StringLiteral "a"] (ListType StringType)
        let expected = ListType StringType
        unify ctx1 (a :| [b]) `shouldBe` Right expected

    describe "checkIsSubTypeOf" $ do
      describe "for lists" $ do
        let emptyListExpr = wctx1 $ List [] EmptyListType
        it "should confirm that an empty list is a subtype of any list" $ do
          checkIsSubTypeOf emptyListExpr EmptyListType `shouldBe` Right ()
          checkIsSubTypeOf emptyListExpr (ListType StringType) `shouldBe` Right ()
          checkIsSubTypeOf emptyListExpr (ListType $ DictType H.empty) `shouldBe` Right ()
        it "should confirm that an empty list is NOT a subtype of a non-list type" $ do
          isLeft (checkIsSubTypeOf emptyListExpr NumberType) `shouldBe` True
          isLeft (checkIsSubTypeOf emptyListExpr (DictType H.empty)) `shouldBe` True
        it "should confirm that a non-empty list is NOT a subtype of an empty list" $ do
          let integerListExpr = wctx1 $ List [wctx2 $ IntegerLiteral 5] (ListType NumberType)
          isLeft (checkIsSubTypeOf integerListExpr EmptyListType) `shouldBe` True
        it "should confirm that a list with elements of type T1 is a subtype of list with elements of type T2 when T1 is a subtype of T2" $ do
          let listOfEmptyLists = wctx1 $ List [wctx2 $ List [] EmptyListType] (ListType EmptyListType)
          checkIsSubTypeOf listOfEmptyLists (ListType $ ListType StringType) `shouldBe` Right ()

      describe "for dictionaries" $ do
        let d1Type = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictRequired NumberType)]
        let d1Expr = wctx1 $ Dict [("a", wctx2 $ BoolLiteral True), ("b", wctx3 $ IntegerLiteral 2)] d1Type

        describe "should confirm that a dict expr D1 is subtype of dict type D2 when" $ do
          it "D2 is type of D1" $ do
            checkIsSubTypeOf d1Expr d1Type `shouldBe` Right ()
          it "D1 contains all fields specified by D2 (and only those), where D2 has some optional fields" $ do
            let d2Type = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
            checkIsSubTypeOf d1Expr d2Type `shouldBe` Right ()
          it "D1 contains all required fields specified by D2 (and only those), where D2 has some optional fields" $ do
            let d2Type =
                  DictType $
                    H.fromList
                      [ ("a", DictRequired BoolType),
                        ("b", DictRequired NumberType),
                        ("c", DictOptional NumberType)
                      ]
            checkIsSubTypeOf d1Expr d2Type `shouldBe` Right ()
          it "D2 has a field of type T1 and D1 has a field of type T2, where T1 is a subtype of T2" $ do
            let d1Type' = DictType $ H.fromList [("a", DictRequired EmptyListType)]
            let d1Expr' = wctx1 $ Dict [("a", wctx2 $ List [] EmptyListType)] d1Type'
            let d2Type' = DictType $ H.fromList [("a", DictRequired $ ListType StringType)]
            checkIsSubTypeOf d1Expr' d2Type' `shouldBe` Right ()

        describe "should confirm that a dict expr D1 is NOT a subtype of dict type D2 when" $ do
          it "D1 contains a field not specified by D2" $ do
            let d2Type = DictType $ H.fromList [("a", DictRequired BoolType)]
            isLeft (checkIsSubTypeOf d1Expr d2Type) `shouldBe` True
          it "D1 does contain a field specified by D2 but has different type" $ do
            let d2Type = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional BoolType)]
            isLeft (checkIsSubTypeOf d1Expr d2Type) `shouldBe` True
          it "D1 does not contain a required field specified by D2" $ do
            let d2Type =
                  DictType $
                    H.fromList
                      [ ("a", DictRequired BoolType),
                        ("b", DictOptional NumberType),
                        ("c", DictRequired NumberType)
                      ]
            isLeft (checkIsSubTypeOf d1Expr d2Type) `shouldBe` True

      it "should fail for non-related types" $ do
        isLeft (checkIsSubTypeOf (wctx1 $ IntegerLiteral 5) StringType) `shouldBe` True
        isLeft (checkIsSubTypeOf (wctx1 $ StringLiteral "a") EmptyListType) `shouldBe` True
        isLeft (checkIsSubTypeOf (wctx1 $ List [wctx2 $ IntegerLiteral 5] (ListType NumberType)) BoolType) `shouldBe` True
        isLeft
          ( checkIsSubTypeOf
              (wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 5)] (DictType H.empty))
              (ListType StringType)
          )
          `shouldBe` True
