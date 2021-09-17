module Analyzer.TypeCheckerTest where

import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker
import qualified Analyzer.TypeDefinitions as TD
import qualified Analyzer.TypeDefinitions.Internal as TD
import Data.Either (isRight)
import qualified Data.HashMap.Strict as H
import Test.Tasty.Hspec

spec_TypeChecker :: Spec
spec_TypeChecker = do
  describe "Analyzer.TypeChecker" $ do
    describe "typeCheck" $ do
      it "Type checks a simple, well-typed example" $ do
        let ast =
              P.AST
                [ P.Decl "app" "Todo" (P.Dict [("title", P.StringLiteral "Todo App")]),
                  P.Decl "app" "Trello" (P.Dict [("title", P.StringLiteral "Trello Clone")])
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
        let ast = P.AST [P.Decl "string" "App" (P.IntegerLiteral 5)]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "string" (TD.DeclType "string" StringType undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expectedError = WeakenError ReasonUncoercable (IntegerLiteral 5) StringType
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
        let ast = P.AST [P.Decl "food" "Cucumber" (P.Var "Dill")]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "food" (TD.DeclType "food" (EnumType "flavor") undefined),
                  TD.enumTypes = H.singleton "flavor" (TD.EnumType "flavor" ["Fresh", "Dill"])
                }
        let actual = typeCheck typeDefs ast
        let expected = Right $ TypedAST [Decl "Cucumber" (Var "Dill" (EnumType "flavor")) (DeclType "food")]
        actual `shouldBe` expected
      it "Type checks an empty list in a declaration" $ do
        let ast = P.AST [P.Decl "rooms" "Bedrooms" (P.List [])]
        let typeDefs =
              TD.TypeDefinitions
                { TD.declTypes = H.singleton "rooms" (TD.DeclType "rooms" (ListType StringType) undefined),
                  TD.enumTypes = H.empty
                }
        let actual = typeCheck typeDefs ast
        let expected = Right $ TypedAST [Decl "Bedrooms" (List [] (ListType StringType)) (DeclType "rooms")]
        actual `shouldBe` expected
