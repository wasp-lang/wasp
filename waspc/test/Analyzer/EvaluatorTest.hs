{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.EvaluatorTest where

import Analyzer.Evaluator
import Analyzer.Evaluator.TH
import Analyzer.Parser (parse)
import Analyzer.Type
import Analyzer.TypeChecker
import qualified Analyzer.TypeDefinitions as TD
import Data.Data (Data)
import qualified Data.HashMap.Strict as H
import Test.Tasty.Hspec

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left e) = error $ show e

newtype Simple = Simple String deriving (Eq, Show, Data)

makeDecl ''Simple

data Fields = Fields {a :: String, b :: Maybe Double} deriving (Eq, Show, Data)

makeDecl ''Fields

data Person = Person {name :: String, age :: Integer} deriving (Eq, Show, Data)

makeDecl ''Person

data BusinessType = Manufacturer | Seller | Store deriving (Eq, Show, Data)

makeEnum ''BusinessType

data Business = Business
  { employees :: [Person],
    worth :: Double,
    businessType :: BusinessType,
    location :: Maybe String
  }
  deriving (Eq, Show, Data)

makeDecl ''Business

spec_Evaluator :: Spec
spec_Evaluator = do
  describe "Analyzer.Evaluator" $
    describe "evaluate" $ do
      it "Evaluates a simple declaration" $ do
        let typeDefs = TD.addDeclType @Simple $ TD.empty
        let typedAst =
              TypedAST
                [Decl "Test" (StringLiteral "hello wasp") (DeclType "simple")]
        fmap takeDecls (evaluate typeDefs typedAst)
          `shouldBe` Right [("Test", Simple "hello wasp")]
      it "Evaluates a declaration with a dictionary" $ do
        let typeDefs = TD.addDeclType @Fields $ TD.empty
        let typedAst =
              TypedAST
                [ Decl
                    "Test"
                    ( Dict
                        [ ("a", StringLiteral "hello wasp"),
                          ("b", DoubleLiteral 3.14)
                        ]
                        (DictType $ H.fromList [("a", DictRequired StringType), ("b", DictOptional NumberType)])
                    )
                    (DeclType "fields")
                ]
        fmap takeDecls (evaluate typeDefs typedAst)
          `shouldBe` Right [("Test", Fields {a = "hello wasp", b = Just 3.14})]
      it "Evaluates a declaration with missing optional fields" $ do
        let typeDefs = TD.addDeclType @Fields $ TD.empty
        let typedAst =
              TypedAST
                [ Decl
                    "Test"
                    ( Dict
                        [ ("a", StringLiteral "hello wasp")
                        ]
                        (DictType $ H.fromList [("a", DictRequired StringType), ("b", DictOptional NumberType)])
                    )
                    (DeclType "fields")
                ]
        fmap takeDecls (evaluate typeDefs typedAst)
          `shouldBe` Right [("Test", Fields {a = "hello wasp", b = Nothing})]
      it "Evaluates a complicated example" $ do
        let typeDefs =
              TD.addDeclType @Business $
                TD.addEnumType @BusinessType $
                  TD.addDeclType @Person $ TD.empty
        let source =
              [ "person Tim { name: \"Tim Stocker\", age: 40 }",
                "person John { name: \"John Cashier\", age: 23 }",
                "business Grocer { employees: [Tim, John], businessType: Store, worth: 115 }"
              ]
        -- Assumes that the source parses and type checks
        let typedAst = fromRight $ typeCheck typeDefs $ fromRight $ parse $ unlines source
        fmap takeDecls (evaluate typeDefs typedAst)
          `shouldBe` Right
            [ ( "Grocer",
                Business
                  { employees =
                      [ Person "Tim Stocker" 40,
                        Person "John Cashier" 23
                      ],
                    businessType = Store,
                    worth = 115.0,
                    location = Nothing
                  }
              )
            ]
