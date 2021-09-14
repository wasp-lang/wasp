{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains combinators for building evaluators to convert
-- TypeChecker.AST into Wasp AST (Wasp.hs).
--
-- In this context, evaluator is a function that takes a piece of TypeChecker.AST,
-- some additional context, and returns a piece of Wasp AST (or error if evaluation fails).
--
-- Evaluator combinator is a function that takes some arguments and returns evaluator.
-- In other words, an evaluator builder.
--
-- This module exposes all the combinators that are needed to build evaluators to parse the
-- whole TypeChecker.AST into Wasp AST.
--
-- Since our evaluation code is created automatically from Wasp AST by the TH functions (TH.hs),
-- these combinators are mostly used there, in the evaluator functions that TH functions generate.
--
-- Evaluator is an instance of Applicative in order to allow easy composing of multiple evaluators.
--
-- An example of usage, where we are building an evalutor @page@ that evalutes a piece of TypeChecker.AST
-- into a @Page@, which is part of the Wasp AST:
--
-- @
-- data Page = Page { title :: String, author :: Maybe String, content :: String }
--
-- page :: Evaluator Page
-- page = dict $ Page <$> field "title" string <*> maybeField "author" string <*> field "content" string
-- @
--
-- This evaluator would turn the Wasp expression
-- @{ title: "Home", content: "Hello world" }@ into
-- @Page { title = "Home", author = Nothing, content = "Hello world" }@
module Analyzer.Evaluator.Combinators
  ( -- * Types
    Evaluator,
    DictEvaluator,

    -- * Functions
    runEvaluator,

    -- * "Evaluator" combinators
    string,
    integer,
    double,
    bool,
    decl,
    enum,
    dict,
    list,
    extImport,
    json,
    psl,

    -- * "DictEvaluator" combinators
    field,
    maybeField,
  )
where

import Analyzer.Evaluator.Decl
import Analyzer.Evaluator.Decl.Operations (fromDecl)
import Analyzer.Evaluator.EvaluationError
import qualified Analyzer.Evaluator.Types as E
import qualified Analyzer.Type as T
import Analyzer.TypeChecker.AST (TypedExpr (..), exprType)
import qualified Analyzer.TypeDefinitions as TD
import Control.Arrow (left)
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.HashMap.Strict as H

-- | Bindings for currently evaluated declarations
type Bindings = H.HashMap String Decl

-- | The context in an evaluation.
type EvalCtx a = (TD.TypeDefinitions, Bindings, a)

-- | An evaluation from "a" to "b" with the evaluation context.
-- We are using `Compose` because it results in an Applicative when it composes two Applicatives,
-- meaning that our evaluators automatically become instance of Applicative.
type (|>) a b = Compose ((->) (EvalCtx a)) (Either EvaluationError) b

-- | An evaluation from a typed expression to a value.
newtype Evaluator a = Evaluator (TypedExpr |> a)
  deriving (Functor, Applicative)

evaluator :: (EvalCtx TypedExpr -> Either EvaluationError a) -> Evaluator a
evaluator = Evaluator . Compose

runEvaluator :: Evaluator a -> TD.TypeDefinitions -> Bindings -> TypedExpr -> Either EvaluationError a
runEvaluator (Evaluator f) typeDefs bindings expr = getCompose f (typeDefs, bindings, expr)

-- | A transformation from dictionary definition (which is a list of dictionary entries) to some type. A "Evaluator" can
-- be created from a "DictEvaluator" with the "dict" combinator.
newtype DictEvaluator a = DictEvaluator ([(String, TypedExpr)] |> a)
  deriving (Functor, Applicative)

dictEvaluator :: (EvalCtx [(String, TypedExpr)] -> Either EvaluationError a) -> DictEvaluator a
dictEvaluator = DictEvaluator . Compose

runDictEvaluator :: DictEvaluator a -> TD.TypeDefinitions -> Bindings -> [(String, TypedExpr)] -> Either EvaluationError a
runDictEvaluator (DictEvaluator f) typeDefs bindings entries = getCompose f (typeDefs, bindings, entries)

-- | An evaluator that expects a "StringLiteral".
string :: Evaluator String
string = evaluator $ \case
  (_, _, StringLiteral str) -> pure str
  (_, _, expr) -> Left $ ExpectedType T.StringType (exprType expr)

-- | An evaluator that expects an "IntegerLiteral" or "DoubleLiteral". A
-- "DoubleLiteral" is rounded to the nearest whole number.
integer :: Evaluator Integer
integer = evaluator $ \case
  (_, _, IntegerLiteral i) -> pure i
  (_, _, DoubleLiteral x) -> pure $ round x
  (_, _, expr) -> Left $ ExpectedType T.NumberType (exprType expr)

-- | An evaluator that expects a "IntegerLiteral" or "DoubleLiteral".
double :: Evaluator Double
double = evaluator $ \case
  (_, _, IntegerLiteral i) -> pure $ fromIntegral i
  (_, _, DoubleLiteral x) -> pure x
  (_, _, expr) -> Left $ ExpectedType T.NumberType (exprType expr)

-- | An evaluator that expects a "BoolLiteral".
bool :: Evaluator Bool
bool = evaluator $ \case
  (_, _, BoolLiteral b) -> pure b
  (_, _, expr) -> Left $ ExpectedType T.BoolType (exprType expr)

-- | An evaluator that expects a "Var" bound to a "Decl" of type "a".
decl :: forall a. TD.IsDeclType a => Evaluator a
decl = evaluator $ \case
  (_, bindings, Var var typ) -> case H.lookup var bindings of
    Nothing -> Left $ UndefinedVariable var
    Just dcl -> case fromDecl @a dcl of
      Nothing -> Left $ ForVariable var (ExpectedType (T.DeclType $ TD.declTypeName @a) typ)
      Just (_, declValue) -> Right declValue
  (_, _, expr) -> Left $ ExpectedType (T.DeclType $ TD.declTypeName @a) (exprType expr)

-- | An evaluator that expects a "Var" bound to an "EnumType" for "a".
enum :: forall a. TD.IsEnumType a => Evaluator a
enum = evaluator $ \case
  (_, _, Var var _) -> TD.enumTypeFromVariant @a var
  (_, _, expr) -> Left $ ExpectedType (T.EnumType $ TD.enumTypeName @a) (exprType expr)

-- | An evaluator that runs a "DictEvaluator". Expects a "Dict" expression and
-- uses its entries to run the "DictEvaluator".
dict :: DictEvaluator a -> Evaluator a
dict dictEvalutor = evaluator $ \case
  (typeDefs, bindings, Dict entries _) -> runDictEvaluator dictEvalutor typeDefs bindings entries
  (_, _, expr) -> Left $ ExpectedDictType $ exprType expr

-- | An evaluator that expects a "List" and runs the inner evaluator on each
-- item in the list.
list :: Evaluator a -> Evaluator [a]
list inner = evaluator $ \case
  (typeDefs, bindings, List values _) -> left InList $ mapM (runEvaluator inner typeDefs bindings) values
  (_, _, expr) -> Left $ ExpectedListType $ exprType expr

-- | An evaluator that expects an "ExtImport".
extImport :: Evaluator E.ExtImport
extImport = evaluator $ \case
  (_, _, ExtImport name file) -> pure $ E.ExtImport name file
  (_, _, expr) -> Left $ ExpectedType T.ExtImportType (exprType expr)

-- | An evaluator that expects a "JSON".
json :: Evaluator E.JSON
json = evaluator $ \case
  (_, _, JSON str) -> pure $ E.JSON str
  (_, _, expr) -> Left $ ExpectedType (T.QuoterType "json") (exprType expr)

-- | An evaluator that expects a "PSL".
psl :: Evaluator E.PSL
psl = evaluator $ \case
  (_, _, PSL str) -> pure $ E.PSL str
  (_, _, expr) -> Left $ ExpectedType (T.QuoterType "psl") (exprType expr)

-- | A dictionary evaluator that requires the field to exist.
field :: String -> Evaluator a -> DictEvaluator a
field key valueEvaluator = dictEvaluator $ \(typeDefs, bindings, entries) -> case lookup key entries of
  Nothing -> Left $ MissingField key
  Just value -> left (InField key) $ runEvaluator valueEvaluator typeDefs bindings value

-- | A dictionary evaluator that allows the field to be missing.
maybeField :: String -> Evaluator a -> DictEvaluator (Maybe a)
maybeField key valueEvaluator = dictEvaluator $ \(typeDefs, bindings, entries) -> case lookup key entries of
  Nothing -> pure Nothing
  Just value -> Just <$> left (InField key) (runEvaluator valueEvaluator typeDefs bindings value)
