{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains combinators for building evaluators for converting
-- "TypedExpr" to arbitrary types.
--
-- A typical use:
--
-- @
-- data Page = Page { title :: String, author :: Maybe String, content :: String }
--
-- page :: Evaluator Page
-- page = dict $ Page <$> field "title" string <*> maybeField "author" string <*> field "content" string
-- @
--
-- This creates a "Evaluator" @page@ that would turn the Wasp expression
-- @{ title: "Home", content: "Hello world" }@ into
-- @Page { title = "Home", author = Nothing, content = "Hello world" }@
module Analyzer.Evaluator.Combinators
  ( -- * Types
    Evaluator,
    DictEvaluator,

    -- * Functions
    build,

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
import Analyzer.Evaluator.EvaluationError
import qualified Analyzer.Evaluator.Types as E
import Analyzer.TypeChecker.AST (TypedExpr (..))
import qualified Analyzer.TypeDefinitions as TD
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.HashMap.Strict as H
import Data.Typeable (cast)

-- | The context in an evaluation.
type EvalCtx a = (TD.TypeDefinitions, H.HashMap String Decl, a)

-- | An evaluation from "a" to "b" with the evaluation context.
type (|>) a b = Compose ((->) (EvalCtx a)) (Either EvaluationError) b

-- | An evaluation from a typed expression to a value.
newtype Evaluator a = Evaluator (TypedExpr |> a)
  deriving (Functor, Applicative)

evaluator :: (EvalCtx TypedExpr -> Either EvaluationError a) -> Evaluator a
evaluator = Evaluator . Compose

runEvaluator :: Evaluator a -> EvalCtx TypedExpr -> Either EvaluationError a
runEvaluator (Evaluator f) = getCompose f

-- | A transformation from a dictionary entry to some type. A "Evaluator" can
-- be created from a "DictEvaluator" with the "dict" combinator.
newtype DictEvaluator a = DictEvaluator ([(String, TypedExpr)] |> a)
  deriving (Functor, Applicative)

dictEvaluator :: (EvalCtx [(String, TypedExpr)] -> Either EvaluationError a) -> DictEvaluator a
dictEvaluator = DictEvaluator . Compose

runDictEvaluator :: DictEvaluator a -> EvalCtx [(String, TypedExpr)] -> Either EvaluationError a
runDictEvaluator (DictEvaluator f) = getCompose f

-- | A convenience function for running a "Evaluator".
build :: Evaluator a -> TD.TypeDefinitions -> H.HashMap String Decl -> TypedExpr -> Either EvaluationError a
build transform typeDefs bindings expr = runEvaluator transform (typeDefs, bindings, expr)

-- | A transform that expects a "StringLiteral".
string :: Evaluator String
string = evaluator $ \case
  (_, _, StringLiteral str) -> pure str
  _ -> Left $ EvaluationError "expected StringType (invalid instance of IsDeclType)"

-- | A transform that expects a "IntegerLiteral" or "DoubleLiteral". A
-- "DoubleLiteral" is rounded to the nearest whole number.
integer :: Evaluator Integer
integer = evaluator $ \case
  (_, _, IntegerLiteral i) -> pure i
  (_, _, DoubleLiteral x) -> pure $ round x
  _ -> Left $ EvaluationError "expected NumberType (invalid instance of IsDeclType)"

-- | A transform that expects a "IntegerLiteral" or "DoubleLiteral".
double :: Evaluator Double
double = evaluator $ \case
  (_, _, IntegerLiteral i) -> pure $ fromIntegral i
  (_, _, DoubleLiteral x) -> pure x
  _ -> Left $ EvaluationError "expected NumberType (invalid instance of IsDeclType)"

-- | A transform that expects a "BoolLiteral".
bool :: Evaluator Bool
bool = evaluator $ \case
  (_, _, BoolLiteral b) -> pure b
  _ -> Left $ EvaluationError "expected BoolType (invalid instance of IsDeclType)"

-- | A transform that expects a "Var" bound to a "Decl" of type "a".
decl :: forall a. TD.IsDeclType a => Evaluator a
decl = evaluator $ \case
  (_, bindings, Var var _) -> case H.lookup var bindings of
    Nothing -> Left $ EvaluationError $ "undefined variable " ++ var
    Just (Decl _ value) -> case cast value :: Maybe a of
      Nothing -> Left $ EvaluationError $ "wrong type for variable " ++ var
      Just a -> Right a
  _ -> Left $ EvaluationError "expected Var (invalid instance of IsDeclType)"

-- | A transform that expects a "Var" bound to an "EnumType" for "a".
enum :: forall a. TD.IsEnumType a => Evaluator a
enum = evaluator $ \case
  (_, _, Var var _) -> let x = TD.enumTypeFromVariant @a var in x
  _ -> Left $ EvaluationError "expected Var (invalid instance of IsEnumType)"

-- | A transform that runs a "DictEvaluator". Expects a "Dict" expression and
-- uses its entries to run the "DictEvaluator".
dict :: DictEvaluator a -> Evaluator a
dict inner = evaluator $ \case
  (typeDefs, bindings, Dict entries _) -> runDictEvaluator inner (typeDefs, bindings, entries)
  _ -> Left $ EvaluationError "Expected DictType (invalid instance of IsDeclType)"

-- | A transform that expects a "List" and runs the "inner" transform on each
-- item in the list.
list :: Evaluator a -> Evaluator [a]
list inner = evaluator $ \case
  (typeDefs, bindings, List values _) -> mapM (\expr -> runEvaluator inner (typeDefs, bindings, expr)) values
  _ -> Left $ EvaluationError "Expected ListType (invalid instance of IsDeclType)"

-- | A transform that expects an "ExtImport".
extImport :: Evaluator E.ExtImport
extImport = evaluator $ \case
  (_, _, ExtImport name file) -> pure $ E.ExtImport name file
  _ -> Left $ EvaluationError "Expected ExtImport (invalid instance of IsDeclType)"

-- | A transform that expects a "JSON".
json :: Evaluator E.JSON
json = evaluator $ \case
  (_, _, JSON str) -> pure $ E.JSON str
  _ -> Left $ EvaluationError "Expected JSON (invalid instance of IsDeclType)"

-- | A transform that expects a "PSL".
psl :: Evaluator E.PSL
psl = evaluator $ \case
  (_, _, PSL str) -> pure $ E.PSL str
  _ -> Left $ EvaluationError "Expected PSL (invalid instance of IsDeclType)"

-- | A dictionary transform that requires the field to exist.
field :: String -> Evaluator a -> DictEvaluator a
field key valueEvaluator = dictEvaluator $ \(typeDefs, bindings, entries) -> case lookup key entries of
  Nothing -> Left $ EvaluationError $ "Missing field " ++ key ++ " (invalid instance of IsDeclType)"
  Just value -> runEvaluator valueEvaluator (typeDefs, bindings, value)

-- | A dictionary transform that allows the field to be missing.
maybeField :: String -> Evaluator a -> DictEvaluator (Maybe a)
maybeField key valueEvaluator = dictEvaluator $ \(typeDefs, bindings, entries) -> case lookup key entries of
  Nothing -> pure Nothing
  Just value -> Just <$> runEvaluator valueEvaluator (typeDefs, bindings, value)
