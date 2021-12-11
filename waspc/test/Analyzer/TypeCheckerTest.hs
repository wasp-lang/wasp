module Analyzer.TypeCheckerTest where

import Analyzer.TestUtil (ctx, wctx)
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
    describe "typeCheck" $ do
      it "Type checks a simple, well-typed example" $ do
        let ast =
              P.AST
                [ wctx 1 1 $ P.Decl "app" "Todo" (wctx 2 1 $ P.Dict [("title", wctx 3 2 $ P.StringLiteral "Todo App")]),
                  wctx 4 1 $ P.Decl "app" "Trello" (wctx 5 2 $ P.Dict [("title", wctx 6 3 $ P.StringLiteral "Trello Clone")])
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
        let ast = P.AST [wctx 1 1 $ P.Decl "string" "App" $ wctx 2 2 $ P.IntegerLiteral 5]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expectedError =
              WeakenError (ctx 1 1) $
                TypeCoercionError (wctx 2 2 $ IntegerLiteral 5) StringType ReasonUncoercable
        actual `shouldBe` Left expectedError
      it "Properly hoists declarations" $ do
        let mAst = P.parse "llnode Head { value: 2, next: Tail } llnode Tail { value: 3 }"
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
        let ast = P.AST [wctx 1 1 $ P.Decl "food" "Cucumber" $ wctx 1 30 $ P.Var "Dill"]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "food" (TD.DeclType "food" (EnumType "flavor") undefined),
                  TD.enumTypes = H.singleton "flavor" (TD.EnumType "flavor" ["Fresh", "Dill"])
                }
        let actual = typeCheck typeDefs ast
        let expected =
              Right $
                TypedAST
                  [ wctx 1 1 $ Decl "Cucumber" (wctx 1 30 $ Var "Dill" (EnumType "flavor")) (DeclType "food")
                  ]
        actual `shouldBe` expected
      it "Type checks an empty list in a declaration" $ do
        let ast = P.AST [wctx 1 1 $ P.Decl "rooms" "Bedrooms" $ wctx 1 30 $ P.List []]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "rooms" (TD.DeclType "rooms" (ListType StringType) undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expected =
              Right $
                TypedAST
                  [ wctx 1 1 $ Decl "Bedrooms" (wctx 1 30 $ List [] (ListType StringType)) (DeclType "rooms")
                  ]
        actual `shouldBe` expected
