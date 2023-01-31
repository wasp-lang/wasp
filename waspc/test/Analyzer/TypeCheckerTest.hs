module Analyzer.TypeCheckerTest where

import Analyzer.TestUtil (ctx)
import Data.Either (isRight)
import qualified Data.HashMap.Strict as H
import Test.Tasty.Hspec
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker
import qualified Wasp.Analyzer.TypeDefinitions as TD
import qualified Wasp.Analyzer.TypeDefinitions.Internal as TD

spec_TypeChecker :: Spec
spec_TypeChecker = do
  describe "Analyzer.TypeChecker" $ do
    let ctx1 = ctx (1, 1) (1, 10)
        ctx2 = ctx (1, 10) (1, 15)
        ctx3 = ctx (2, 5) (3, 10)
        ctx4 = ctx (2, 12) (2, 20)
        ctx5 = ctx (2, 20) (2, 25)
        ctx6 = ctx (3, 3) (3, 18)
        wctx1 = WithCtx ctx1
        wctx2 = WithCtx ctx2
        wctx3 = WithCtx ctx3
        wctx4 = WithCtx ctx4
        wctx5 = WithCtx ctx5
        wctx6 = WithCtx ctx6

    describe "typeCheck" $ do
      it "Type checks a simple, well-typed example" $ do
        let ast =
              P.AST
                [ wctx1 $ P.Decl "app" "Todo" (wctx2 $ P.Dict [("title", wctx3 $ P.StringLiteral "Todo App")]),
                  wctx4 $ P.Decl "app" "Trello" (wctx5 $ P.Dict [("title", wctx6 $ P.StringLiteral "Trello Clone")])
                ]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes =
                    H.fromList
                      [ ( "app",
                          TD.DeclType
                            "app"
                            ( DictType $
                                H.fromList
                                  [ ("title", DictOptional StringType)
                                  ]
                            )
                            undefined
                        )
                      ],
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        actual `shouldSatisfy` isRight
      it "Fails to type check a simple, ill-typed example" $ do
        let ast = P.AST [wctx1 $ P.Decl "string" "App" $ wctx2 $ P.IntegerLiteral 5]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expectedError =
              mkTypeError ctx1 $
                WeakenError $
                  TypeCoercionError (wctx2 $ IntegerLiteral 5) StringType ReasonUncoercable
        actual `shouldBe` Left expectedError
      it "Properly hoists declarations" $ do
        let mAst = P.parseStatements "llnode Head { value: 2, next: Tail } llnode Tail { value: 3 }"
        mAst `shouldSatisfy` isRight
        let (Right ast) = mAst
        let llnodeArgType =
              DictType $
                H.fromList
                  [("value", DictRequired NumberType), ("next", DictOptional $ DeclType "llnode")]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "llnode" (TD.DeclType "llnode" llnodeArgType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        actual `shouldSatisfy` isRight
      it "Type checks an existing enum value" $ do
        let ast = P.AST [wctx1 $ P.Decl "food" "Cucumber" $ wctx2 $ P.Var "Dill"]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "food" (TD.DeclType "food" (EnumType "flavor") undefined),
                  TD.enumTypes = H.singleton "flavor" (TD.EnumType "flavor" ["Fresh", "Dill"])
                }
        let actual = typeCheck typeDefs ast
        let expected =
              Right $
                TypedAST
                  [ wctx1 $ Decl "Cucumber" (wctx2 $ Var "Dill" (EnumType "flavor")) (DeclType "food")
                  ]
        actual `shouldBe` expected
      it "Type checks an empty list in a declaration" $ do
        let ast = P.AST [wctx1 $ P.Decl "rooms" "Bedrooms" $ wctx2 $ P.List []]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "rooms" (TD.DeclType "rooms" (ListType StringType) undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expected =
              Right $
                TypedAST
                  [ wctx1 $ Decl "Bedrooms" (wctx2 $ List [] EmptyListType) (DeclType "rooms")
                  ]
        actual `shouldBe` expected
