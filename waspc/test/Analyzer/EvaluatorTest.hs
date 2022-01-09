{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.EvaluatorTest where

import Data.Data (Data)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Text.Read (readMaybe)
import Wasp.Analyzer.Evaluator
import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Evaluator.EvaluationError as EvaluationError
import Wasp.Analyzer.Parser (parse)
import qualified Wasp.Analyzer.Type as T
import Wasp.Analyzer.TypeChecker (typeCheck)
import qualified Wasp.Analyzer.TypeChecker.AST as TypedAST
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation (..))
import Wasp.Analyzer.TypeDefinitions.TH
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.ExtImport (ExtImport (..), ExtImportName (..))
import Wasp.AppSpec.JSON (JSON (..))

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left e) = error $ show e

------- Simple -------

newtype Simple = Simple String deriving (Eq, Show, Data)

instance IsDecl Simple

makeDeclType ''Simple

------- Fields -------

data Fields = Fields {a :: String, b :: Maybe Double} deriving (Eq, Show, Data)

instance IsDecl Fields

makeDeclType ''Fields

------ Business ------

data Person = Person {name :: String, age :: Integer} deriving (Eq, Show, Data)

instance IsDecl Person

makeDeclType ''Person

data BusinessType = Manufacturer | Seller | Store deriving (Eq, Show, Data)

makeEnumType ''BusinessType

data Business = Business
  { employees :: [Ref Person],
    worth :: Double,
    businessType :: BusinessType,
    location :: Maybe String
  }
  deriving (Eq, Show, Data)

instance IsDecl Business

makeDeclType ''Business

-------- Special --------

data Special = Special {imps :: [ExtImport], json :: JSON} deriving (Eq, Show)

instance IsDecl Special

makeDeclType ''Special

------ HasCustomEvaluation ------

data SemanticVersion = SemanticVersion Int Int Int
  deriving (Eq, Show, Data)

instance HasCustomEvaluation SemanticVersion where
  waspType = T.StringType
  evaluation = E.evaluation' . TypedAST.withCtx $ \ctx -> \case
    TypedAST.StringLiteral str -> case splitOn "." str of
      [major, minor, patch] ->
        maybe
          ( Left $
              EvaluationError.mkEvaluationError ctx $
                EvaluationError.ParseError $
                  EvaluationError.EvaluationParseError
                    "Failed parsing semantic version -> some part is not int"
          )
          pure
          $ do
            majorInt <- readMaybe @Int major
            minorInt <- readMaybe @Int minor
            patchInt <- readMaybe @Int patch
            return $ SemanticVersion majorInt minorInt patchInt
      _ ->
        Left $
          EvaluationError.mkEvaluationError ctx $
            EvaluationError.ParseError $
              EvaluationError.EvaluationParseError
                "Failed parsing semantic version -> it doesn't have 3 comma separated parts."
    expr ->
      Left $
        EvaluationError.mkEvaluationError ctx $
          EvaluationError.ExpectedType T.StringType (TypedAST.exprType expr)

data Custom = Custom
  {version :: SemanticVersion}
  deriving (Eq, Show, Data)

instance IsDecl Custom

makeDeclType ''Custom

--------------------------------

------ Tuples ------
data Tuples = Tuples
  { pair :: (String, Integer),
    triple :: (String, Integer, Integer),
    quadruple :: (String, Integer, Integer, [Bool])
  }
  deriving (Eq, Show, Data)

instance IsDecl Tuples

makeDeclType ''Tuples

--------------------

eval :: TD.TypeDefinitions -> [String] -> Either EvaluationError [Decl]
eval typeDefs source = evaluate typeDefs $ fromRight $ typeCheck typeDefs $ fromRight $ parse $ unlines source

spec_Evaluator :: Spec
spec_Evaluator = do
  describe "Analyzer.Evaluator" $
    describe "evaluate" $ do
      it "Evaluates a simple declaration" $ do
        let typeDefs = TD.addDeclType @Simple $ TD.empty
        let decls = eval typeDefs ["simple Test \"hello wasp\""]
        fmap takeDecls decls
          `shouldBe` Right [("Test", Simple "hello wasp")]
      it "Evaluates a declaration with a dictionary" $ do
        let typeDefs = TD.addDeclType @Fields $ TD.empty
        let decls = eval typeDefs ["fields Test { a: \"hello wasp\", b: 3.14 }"]
        fmap takeDecls decls
          `shouldBe` Right [("Test", Fields {a = "hello wasp", b = Just 3.14})]
      it "Evaluates a declaration with missing optional fields" $ do
        let typeDefs = TD.addDeclType @Fields $ TD.empty
        let decls = eval typeDefs ["fields Test { a: \"hello wasp\" }"]
        fmap takeDecls decls
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
        fmap takeDecls (eval typeDefs source)
          `shouldBe` Right
            [ ( "Grocer",
                Business
                  { employees = [Ref "Tim", Ref "John"],
                    businessType = Store,
                    worth = 115.0,
                    location = Nothing
                  }
              )
            ]
      it "Evaluates ExtImports and JSON" $ do
        let typeDefs = TD.addDeclType @Special $ TD.empty
        let source =
              [ "special Test {",
                "  imps: [import { field } from \"main.js\", import main from \"main.js\"],",
                "  json: {=json \"key\": 1 json=}",
                "}"
              ]

        fmap takeDecls (eval typeDefs source)
          `shouldBe` Right
            [ ( "Test",
                Special
                  [ ExtImport (ExtImportField "field") (fromJust $ SP.parseRelFileP "main.js"),
                    ExtImport (ExtImportModule "main") (fromJust $ SP.parseRelFileP "main.js")
                  ]
                  (JSON " \"key\": 1 ")
              )
            ]

      it "Evaluates a declaration with a field that has custom evaluation" $ do
        let typeDefs = TD.addDeclType @Custom $ TD.empty
        let decls = eval typeDefs ["custom Test { version: \"1.2.3\" }"]
        fmap takeDecls decls
          `shouldBe` Right [("Test", Custom {version = SemanticVersion 1 2 3})]

      it "Evaluates a declaration with fields that are tuples" $ do
        let typeDefs = TD.addDeclType @Tuples $ TD.empty
        let source =
              [ "tuples Tuples {",
                "  pair: (\"foo\", 1),",
                "  triple: (\"foo\", 1, 2),",
                "  quadruple: (\"foo\", 1, 2, [true, false])",
                "}"
              ]
        fmap takeDecls (eval typeDefs source)
          `shouldBe` Right
            [ ( "Tuples",
                Tuples
                  { pair = ("foo", 1),
                    triple = ("foo", 1, 2),
                    quadruple = ("foo", 1, 2, [True, False])
                  }
              )
            ]
