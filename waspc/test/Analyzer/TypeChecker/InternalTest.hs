module Analyzer.TypeChecker.InternalTest where

import Analyzer.TestUtil (ctx, fromWithCtx)
import Data.Either (fromRight, isLeft)
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Text.Printf (printf)
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.AST
import Wasp.Analyzer.TypeChecker.Internal
import Wasp.Analyzer.TypeChecker.Monad (Bindings, run, runWithBound)
import Wasp.Analyzer.TypeChecker.TypeError
  ( TypeCoercionError (TypeCoercionError),
    TypeCoercionErrorReason (ReasonUncoercable),
    TypeError,
    TypeError'
      ( DictDuplicateField,
        NoDeclarationType,
        QuoterUnknownTag,
        UndefinedIdentifier,
        WeakenError
      ),
    mkTypeError,
  )
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
  -- TODO: The test does not test whether inferExprType returns the correct expression,
  -- it only looks at its type
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

    describe "unifyTypes" $ do
      describe "Unifies two same types to themselves: a | a = a" $ do
        let performTest = \t ->
              it (show t) $
                unifyTypes t t `shouldBe` t
        mapM_
          performTest
          [ NumberType,
            DictType $ H.fromList [("a", DictRequired BoolType)],
            StringType,
            ListType StringType,
            UnionType NumberType StringType
          ]

      it "Unifies a list of type [a] and an empty list into a list of type [a]: [a] | [] = [a]" $ do
        let listType = ListType StringType
        unifyTypes listType EmptyListType `shouldBe` listType
        unifyTypes EmptyListType listType `shouldBe` listType

      describe "Unifies two different types by just constructing their union type" $ do
        let performTest = \(t1, t2) ->
              it (printf "For `%s` and `%s`" (show t1) (show t2)) $
                unifyTypes t1 t2 `shouldBe` makeUnionType t1 t2
        mapM_
          performTest
          [ (NumberType, StringType),
            (DictType $ H.fromList [("a", DictRequired BoolType)], DictType $ H.fromList [("a", DictRequired NumberType)]),
            (NumberType, DictType $ H.fromList [("a", DictRequired BoolType)]),
            (UnionType NumberType StringType, UnionType StringType BoolType)
          ]

    -- TODO: test makeUnionType.

    describe "unify" $ do
      it "Doesn't affect 2 expressions of the same type" $ do
        property $ \(a, b) ->
          let texprs = wctx2 (IntegerLiteral a) :| [wctx3 $ DoubleLiteral b]
              superType = NumberType
           in unify texprs == (texprs, superType)
      it "Unifies two same-typed dictionaries to their original type" $ do
        let superType = DictType $ H.fromList [("a", DictRequired BoolType), ("b", DictOptional NumberType)]
        let a = wctx2 $ Dict [("a", wctx3 $ BoolLiteral True), ("b", wctx4 $ IntegerLiteral 2)] superType
        let b = wctx5 $ Dict [("a", wctx6 $ BoolLiteral True), ("b", wctx7 $ DoubleLiteral 3.14)] superType
        let texprs = a :| [b]
        unify texprs `shouldBe` (texprs, superType)
      it "Unifies an empty dict and a dict with one property into union type" $ do
        let emptyDictTypedExpr = wctx2 $ Dict [] emptyDictTypedExprType
            emptyDictTypedExprType = DictType H.empty
        let onePropDictTypedExpr = wctx3 $ Dict [("a", wctx4 $ BoolLiteral True)] onePropDictTypedExprType
            onePropDictTypedExprType = DictType $ H.singleton "a" $ DictRequired BoolType
        let expectedSuperType = unifyTypes emptyDictTypedExprType onePropDictTypedExprType
        let texprs = emptyDictTypedExpr :| [onePropDictTypedExpr]
        unify texprs
          `shouldBe` ( fromRight (error "Should not happen") . weaken expectedSuperType <$> texprs,
                       expectedSuperType
                     )
      -- TODO: Remove once we make it so that unify doesn't weaken the texprs it unifies, since idempotency then
      --   makes no sense as a property.
      it "Is idempotent when unifying a list of empty dict and a singleton dict" $ do
        let a = wctx2 $ Dict [] (DictType H.empty)
        let b = wctx3 $ Dict [("a", wctx4 $ BoolLiteral True)] $ DictType $ H.singleton "a" $ DictRequired BoolType
        unify (a :| [b]) `shouldBe` unify (fst (unify (a :| [b])))
      it "Unifies an empty list with any other list" $ do
        let emptyList = wctx2 $ List [] EmptyListType
        let nonEmptyListType = ListType StringType
        let nonEmptyList = wctx3 $ List [wctx4 $ StringLiteral "a"] nonEmptyListType
        let expectedSuperType = unifyTypes EmptyListType nonEmptyListType
        let texprs = emptyList :| [nonEmptyList]
        unify texprs
          `shouldBe` ( fromRight (error "Should not happen") . weaken expectedSuperType <$> texprs,
                       expectedSuperType
                     )
    describe "weaken" $ do
      it "Should correctly weaken a simple expression to its supertype" $ do
        weaken NumberType (wctx1 $ DoubleLiteral 1.16) `shouldBe` Right (wctx1 $ DoubleLiteral 1.16)
      it "Should return an type coercion error when the provided type is not the expression's supertype" $ do
        -- TODO: test the returned error more thoroughly
        isLeft (weaken StringType (wctx1 $ DoubleLiteral 1.16)) `shouldBe` True
        isLeft
          ( weaken
              (TupleType (StringType, NumberType, []))
              (wctx1 $ Tuple (wctx2 $ StringLiteral "foo", wctx3 $ BoolLiteral True, []) (TupleType (StringType, BoolType, [])))
          )
          `shouldBe` True

      -- TODO: property testing.
      describe "Correctly weakens complex types (list, dictionary, tuple)" $ do
        it "Should weaken empty list to any list type" $ do
          weaken (ListType StringType) (wctx1 $ List [] EmptyListType) `shouldBe` Right (wctx1 $ List [] StringType)

        it "Should weaken [T] to [T'] if T' is supertype of T" $ do
          let superType = makeUnionType StringType subType
              subType = BoolType
          weaken (ListType superType) (wctx1 $ List [] subType) `shouldBe` Right (wctx1 $ List [] superType)

        -- TODO: we should test this for many possible different combinations
        it "Should weaken (T, _) to (T', _) if T' is supertype of T" $ do
          let texpr1 = wctx2 $ BoolLiteral True
              texpr2 = wctx3 $ List [] EmptyListType
              superType = makeUnionType StringType (exprType $ fromWithCtx texpr1)
              texpr1Type = exprType $ fromWithCtx texpr1
              texpr2Type = exprType $ fromWithCtx texpr2
          weaken
            (TupleType (superType, texpr2Type, []))
            (wctx1 $ Tuple (texpr1, texpr2, []) (TupleType (texpr1Type, texpr2Type, [])))
            `shouldBe` Right (wctx1 $ Tuple (texpr1, texpr2, []) (TupleType (superType, texpr2Type, [])))

      -- { a: bool | string } > { a : string }
      --
      -- testing optional dictionary fields
      it "Should fail when weakening { a?: T } to { a: T }" $ do
        let subtype = DictType $ H.fromList [("a", DictOptional NumberType)]
            supertype = DictType $ H.fromList [("a", DictRequired NumberType)]
            mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
        -- TODO: make test more informative (i.e., test for error message)
        isLeft (weaken supertype (mkTexprWithType subtype)) `shouldBe` True
      it "Should successfully weaken { a: T } to { a?: T }" $ do
        let subtype = DictType $ H.fromList [("a", DictRequired NumberType)]
            supertype = DictType $ H.fromList [("a", DictOptional NumberType)]
            mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
        weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)
      it "Should fail when weakening { } to { a: T }" $ do
        let subtype = DictType $ H.fromList []
            supertype = DictType $ H.fromList [("a", DictRequired NumberType)]
            mkTexprWithType t = wctx1 $ Dict [] t
        isLeft (weaken supertype (mkTexprWithType subtype)) `shouldBe` True
      -- NOTE: We could have allowed this, similar as Typescript does (structural typing),
      --   but we decided to not allow it to avoid developer accidentaly defining unexisting fields
      --   and thinking they do something.
      it "Should fail when weakening { a: T } to { }" $ do
        let subtype = DictType $ H.fromList [("a", DictRequired NumberType)]
            supertype = DictType $ H.fromList []
            mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
        isLeft (weaken supertype (mkTexprWithType subtype)) `shouldBe` True
      it "Should successfully weaken { } to { a?: T }" $ do
        let subtype = DictType $ H.fromList []
            supertype = DictType $ H.fromList [("a", DictOptional NumberType)]
            mkTexprWithType t = wctx1 $ Dict [] t
        weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)
      it "Should fail when weakening { a?: T } to { }" $ do
        let subtype = DictType $ H.fromList [("a", DictOptional NumberType)]
            supertype = DictType $ H.fromList []
            mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
        isLeft (weaken supertype (mkTexprWithType subtype)) `shouldBe` True
      describe "Dictionary D' is supertype of dictionary D if fields of D' are supertypes of fields of D." $ do
        -- testing supertypes in dictionary fields (transparency of dictionary).
        it "Should successfully weaken { a: T } to { a: T' } if T' is supertype of T" $ do
          let subtype = DictType $ H.fromList [("a", DictRequired NumberType)]
              supertype = DictType $ H.fromList [("a", DictRequired $ makeUnionType StringType NumberType)]
              mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
          weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)
        it "Should successfully weaken { a?: T } to { a?: T' } if T' is supertype of T" $ do
          let subtype = DictType $ H.fromList [("a", DictOptional NumberType)]
              supertype = DictType $ H.fromList [("a", DictOptional $ makeUnionType StringType NumberType)]
              mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
          weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)
        it "Should successfully weaken { a: T } to { a?: T' } if T' is supertype of T" $ do
          let subtype = DictType $ H.fromList [("a", DictRequired NumberType)]
              supertype = DictType $ H.fromList [("a", DictOptional $ makeUnionType StringType NumberType)]
              mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
          weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)
        it "Should successfully weaken { a?: T } to { a: T' } if T' is supertype of T" $ do
          let subtype = DictType $ H.fromList [("a", DictOptional NumberType)]
              supertype = DictType $ H.fromList [("a", DictRequired $ makeUnionType StringType NumberType)]
              mkTexprWithType t = wctx1 $ Dict [("a", wctx2 $ IntegerLiteral 2)] t
          isLeft (weaken supertype (mkTexprWithType subtype)) `shouldBe` True

      it "Should successfully weaken a complex dictionary" $ do
        let subtype =
              DictType $
                H.fromList
                  [ ("a", DictRequired $ makeUnionType NumberType StringType),
                    ("b", DictRequired BoolType),
                    ("c", DictRequired subtypeInnerDictType)
                  ]
            subtypeInnerDictType =
              DictType $
                H.fromList
                  [ ("1", DictRequired BoolType),
                    ("2", DictOptional $ ListType StringType)
                  ]
            supertype =
              DictType $
                H.fromList
                  [ ("a", DictRequired $ makeUnionType BoolType (makeUnionType NumberType StringType)),
                    ("b", DictOptional $ makeUnionType BoolType StringType),
                    ( "c",
                      DictRequired $
                        DictType $
                          H.fromList
                            [ ("1", DictRequired BoolType),
                              ("2", DictOptional $ ListType StringType),
                              ("3", DictOptional NumberType)
                            ]
                    )
                  ]
            mkTexprWithType t =
              wctx1 $
                Dict
                  [ ("a", wctx1 $ IntegerLiteral 2),
                    ("b", wctx2 $ BoolLiteral True),
                    ("c", wctx3 $ Dict [("1", wctx4 $ BoolLiteral False)] subtypeInnerDictType)
                  ]
                  t
        weaken supertype (mkTexprWithType subtype) `shouldBe` Right (mkTexprWithType supertype)

    -- TODO: test weaken with complex types (e.g., dicts, lists)
    -- TODO: it seems that weaken currently doesn't support tuples
    -- (e.g., we cannot weaken (bool, string) to (bool, string | number))
    -- This is probably not the behavior we want, since all type constructors (i.e., lists, tuples, dicts)
    -- should behave consistently.

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
      testSuccess "Types quoted psl as PSLType" (wctx1 $ P.Quoter "psl" "id Int @id") (QuoterType "psl")
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
      testSuccess
        "Type checks a list containing strings and numbers"
        (wctx1 $ P.List [wctx2 $ P.IntegerLiteral 5, wctx3 $ P.StringLiteral "4"])
        -- TODO: decide on a representation for union types (e.g., set, list, tree, all types are unions)
        -- and correctly implement the equality instance.
        (ListType (UnionType StringType NumberType))

      testSuccess
        "Creates a union when unifying a list of dictionaries with different fields"
        ( wctx1 $
            P.List
              [ wctx2 $ P.Dict [("a", wctx3 $ P.IntegerLiteral 5)],
                wctx4 $ P.Dict [],
                wctx5 $ P.Dict [("b", wctx6 $ P.StringLiteral "string")]
              ]
        )
        ( ListType $
            UnionType
              (DictType (H.fromList []))
              ( UnionType
                  (DictType $ H.fromList [("b", DictRequired StringType)])
                  (DictType $ H.fromList [("a", DictRequired NumberType)])
              )
        )
      testSuccess
        "Creates a union when unifying two dictionaries with the same field with a different type"
        ( wctx1 $
            P.List
              [ wctx2 $ P.Dict [("a", wctx3 $ P.IntegerLiteral 5)],
                wctx4 $ P.Dict [("a", wctx5 $ P.StringLiteral "string")]
              ]
        )
        ( ListType $
            UnionType
              (DictType $ H.fromList [("a", DictRequired StringType)])
              (DictType $ H.fromList [("a", DictRequired NumberType)])
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
                WeakenError $
                  TypeCoercionError
                    (wctx2 $ IntegerLiteral 5)
                    StringType
                    ReasonUncoercable
        actual `shouldBe` Left expectedError
      it "Type checks declaration with dict type with an argument that unifies to the correct type" $ do
        let ast = wctx1 $ P.Decl "maybeString" "App" $ wctx2 $ P.Dict [("val", wctx3 $ P.StringLiteral "Wasp")]
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
                wctx1 $
                  Decl
                    "App"
                    ( wctx2 $
                        Dict
                          [("val", wctx3 $ StringLiteral "Wasp")]
                          (DictType $ H.singleton "val" (DictOptional StringType))
                    )
                    (DeclType "maybeString")
        actual `shouldBe` expected
