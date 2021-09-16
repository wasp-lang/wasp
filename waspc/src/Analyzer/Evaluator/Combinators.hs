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
  ( -- * Functions
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
import Analyzer.Evaluator.EvaluationError (EvaluationError)
import qualified Analyzer.Evaluator.EvaluationError as EvaluationError
import qualified Analyzer.Evaluator.Types as E
import qualified Analyzer.Type as T
import qualified Analyzer.TypeChecker.AST as TypedAST
import qualified Analyzer.TypeDefinitions as TD
import Control.Arrow (left)
import Data.Functor.Compose (Compose (Compose, getCompose))
import qualified Data.HashMap.Strict as H

-- TODO: Split into TypedExprEvaluator and DictEvaluator?

-- | Bindings for declarations that have been evaluated so far.
type Bindings = H.HashMap DeclName Decl

type DeclName = String

-- | The context in an evaluation.
type EvalCtx = (TD.TypeDefinitions, Bindings)

-- | An evaluation of "a" into "b". It has evaluation context and it can return an evaluation error.
-- We are using `Compose` because it results in an Applicative when it composes two Applicatives,
-- meaning that our evaluators automatically become instance of Applicative.
newtype Evaluator a b = Evaluator (Compose ((->) EvalCtx) (Compose ((->) a) (Either EvaluationError)) b)
  deriving (Functor, Applicative)

evaluator :: (EvalCtx -> a -> Either EvaluationError b) -> Evaluator a b
evaluator f = Evaluator $ Compose $ \ctx -> Compose $ f ctx

evaluator' :: (a -> Either EvaluationError b) -> Evaluator a b
evaluator' = evaluator . const

runEvaluator :: Evaluator a b -> TD.TypeDefinitions -> Bindings -> a -> Either EvaluationError b
runEvaluator (Evaluator f) typeDefs bindings = getCompose (getCompose f (typeDefs, bindings))

type TypedExprEvaluator a = Evaluator TypedAST.TypedExpr a

-- | A transformation from dictionary definition (which is a list of dictionary entries) to some type. A "Evaluator" can
-- be created from a "DictEvaluator" with the "dict" combinator.
type DictEvaluator a = Evaluator TypedDictEntries a

newtype TypedDictEntries = TypedDictEntries [(String, TypedAST.TypedExpr)]

-- | An evaluator that expects a "StringLiteral".
string :: TypedExprEvaluator String
string = evaluator' $ \case
  TypedAST.StringLiteral str -> pure str
  expr -> Left $ EvaluationError.ExpectedType T.StringType (TypedAST.exprType expr)

-- | An evaluator that expects an "IntegerLiteral" or "DoubleLiteral". A
-- "DoubleLiteral" is rounded to the nearest whole number.
integer :: TypedExprEvaluator Integer
integer = evaluator' $ \case
  TypedAST.IntegerLiteral i -> pure i
  TypedAST.DoubleLiteral x -> pure $ round x
  expr -> Left $ EvaluationError.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluator that expects a "IntegerLiteral" or "DoubleLiteral".
double :: TypedExprEvaluator Double
double = evaluator' $ \case
  TypedAST.IntegerLiteral i -> pure $ fromIntegral i
  TypedAST.DoubleLiteral x -> pure x
  expr -> Left $ EvaluationError.ExpectedType T.NumberType (TypedAST.exprType expr)

-- | An evaluator that expects a "BoolLiteral".
bool :: TypedExprEvaluator Bool
bool = evaluator' $ \case
  TypedAST.BoolLiteral b -> pure b
  expr -> Left $ EvaluationError.ExpectedType T.BoolType (TypedAST.exprType expr)

-- | An evaluator that expects a "Var" bound to a "Decl" of type "a".
decl :: forall a. TD.IsDeclType a => TypedExprEvaluator a
decl = evaluator $ \(_, bindings) -> \case
  TypedAST.Var var typ -> case H.lookup var bindings of
    Nothing -> Left $ EvaluationError.UndefinedVariable var
    Just dcl -> case fromDecl @a dcl of
      Nothing ->
        Left $
          EvaluationError.WithContext
            (EvaluationError.ForVariable var)
            (EvaluationError.ExpectedType (T.DeclType declTypeName) typ)
      Just (_dclName, dclValue) -> Right dclValue
  expr -> Left $ EvaluationError.ExpectedType (T.DeclType declTypeName) (TypedAST.exprType expr)
  where
    declTypeName = TD.dtName $ TD.declType @a

-- | An evaluator that expects a "Var" bound to an "EnumType" for "a".
enum :: forall a. TD.IsEnumType a => TypedExprEvaluator a
enum = evaluator' $ \case
  TypedAST.Var var _ -> TD.enumTypeFromVariant @a var
  expr -> Left $ EvaluationError.ExpectedType (T.EnumType $ TD.etName $ TD.enumType @a) (TypedAST.exprType expr)

-- | An evaluator that runs a "DictEvaluator". Expects a "Dict" expression and
-- uses its entries to run the "DictEvaluator".
dict :: DictEvaluator a -> TypedExprEvaluator a
dict dictEvalutor = evaluator $ \(typeDefs, bindings) -> \case
  TypedAST.Dict entries _ -> runEvaluator dictEvalutor typeDefs bindings $ TypedDictEntries entries
  expr -> Left $ EvaluationError.ExpectedDictType $ TypedAST.exprType expr

-- | An evaluator that expects a "List" and runs the inner evaluator on each
-- item in the list.
list :: TypedExprEvaluator a -> TypedExprEvaluator [a]
list elemEvaluator = evaluator $ \(typeDefs, bindings) -> \case
  TypedAST.List values _ ->
    left (EvaluationError.WithContext EvaluationError.InList) $
      mapM (runEvaluator elemEvaluator typeDefs bindings) values
  expr -> Left $ EvaluationError.ExpectedListType $ TypedAST.exprType expr

-- | An evaluator that expects an "ExtImport".
extImport :: TypedExprEvaluator E.ExtImport
extImport = evaluator' $ \case
  TypedAST.ExtImport name file -> pure $ E.ExtImport name file
  expr -> Left $ EvaluationError.ExpectedType T.ExtImportType (TypedAST.exprType expr)

-- | An evaluator that expects a "JSON".
json :: TypedExprEvaluator E.JSON
json = evaluator' $ \case
  TypedAST.JSON str -> pure $ E.JSON str
  expr -> Left $ EvaluationError.ExpectedType (T.QuoterType "json") (TypedAST.exprType expr)

-- | An evaluator that expects a "PSL".
psl :: TypedExprEvaluator E.PSL
psl = evaluator' $ \case
  TypedAST.PSL str -> pure $ E.PSL str
  expr -> Left $ EvaluationError.ExpectedType (T.QuoterType "psl") (TypedAST.exprType expr)

-- | A dictionary evaluator that requires the field to exist.
field :: String -> TypedExprEvaluator a -> DictEvaluator a
field key valueEvaluator = evaluator $
  \(typeDefs, bindings) (TypedDictEntries entries) -> case lookup key entries of
    Nothing -> Left $ EvaluationError.MissingField key
    Just value ->
      left (EvaluationError.WithContext (EvaluationError.InField key)) $
        runEvaluator valueEvaluator typeDefs bindings value

-- | A dictionary evaluator that allows the field to be missing.
maybeField :: String -> TypedExprEvaluator a -> DictEvaluator (Maybe a)
maybeField key valueEvaluator = evaluator $
  \(typeDefs, bindings) (TypedDictEntries entries) -> case lookup key entries of
    Nothing -> pure Nothing
    Just value ->
      Just
        <$> left
          (EvaluationError.WithContext (EvaluationError.InField key))
          (runEvaluator valueEvaluator typeDefs bindings value)
