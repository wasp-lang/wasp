{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | This module implements the type rules defined by the wasplang document
-- in two phases.
--
-- First, the bindings created by declaration statements are hoisted. Next, the
-- types of the argument to each declaration is checked to see if it matches the
-- type definition's corresponding "Analyzer.TypeDefintions.DeclType".
--
-- = Hoisting Declarations
--
-- The process of hoisting declarations is simple: for each declaration in the
-- AST, the name of the declaration is bound to the correct "Analyzer.Type.DeclType", without
-- checking the type of its argument.
--
-- For example, the wasp source file
--
-- @
-- app Todo {
--   title: "Todo App"
-- }
--
-- page Home {
--   component: import HomePage from '@ext/Home.jsx'
-- }
-- @
--
-- would have the bindings @[("Todo", DeclType "app"), ("Home", DeclType "page")] after
-- this phase.
--
-- = Statement Type Checking
--
-- In this phase, the type rules are applied to the @expr@ of each @Decl typeName name expr@ to
-- determine it's type. That type is then compared to the type of @typeName@ to determine
-- if the statement is well-typed.
--
-- For the implementation of the type inference rules, see "checkExpr".
module Analyzer.TypeChecker.Internal
  ( check,
    hoistDeclarations,
    checkAST,
    checkStmt,
    checkExpr,
    unify,
    unifyTypes,
    weaken,
  )
where

import Analyzer.Parser (AST)
import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker.AST
import Analyzer.TypeChecker.Monad
import Analyzer.TypeChecker.TypeError
import qualified Analyzer.TypeDefinitions as TD
import Control.Arrow (left)
import Control.Monad (foldM)
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)

check :: AST -> T TypedAST
check ast = hoistDeclarations ast >> checkAST ast

hoistDeclarations :: AST -> T ()
hoistDeclarations (P.AST stmts) = mapM_ hoistDeclaration stmts
  where
    hoistDeclaration :: P.Stmt -> T ()
    hoistDeclaration (P.Decl typName ident _) =
      lookupDecl typName >>= \case
        Nothing -> throw $ NoDeclarationType typName
        Just _ -> setType ident $ DeclType typName

checkAST :: AST -> T TypedAST
checkAST (P.AST stmts) = TypedAST <$> mapM checkStmt stmts

-- | Checks that statements have valid types
checkStmt :: P.Stmt -> T TypedStmt
checkStmt (P.Decl typName name expr) =
  lookupDecl typName >>= \case
    Nothing -> throw $ NoDeclarationType typName
    Just (TD.DeclType _ expectedType) -> do
      -- Applies type inference on rules on the expression to try to prove that
      -- "Γ ⊢ expr : expectedType"
      mTypedExpr <- weaken expectedType <$> checkExpr expr
      case mTypedExpr of
        Left e -> throw e
        Right typedExpr -> return $ Decl name typedExpr (DeclType typName)

-- | Determine the type of an expression, following the inference rules described in
-- the wasplang document. Some these rules are referenced by name in the comments
-- of the following functions using [Brackets].
checkExpr :: P.Expr -> T TypedExpr
-- Literals are straight forward: each one can have one type
checkExpr (P.StringLiteral s) = return $ StringLiteral s
checkExpr (P.IntegerLiteral i) = return $ IntegerLiteral i
checkExpr (P.DoubleLiteral d) = return $ DoubleLiteral d
checkExpr (P.BoolLiteral b) = return $ BoolLiteral b
checkExpr (P.ExtImport n s) = return $ ExtImport n s
-- For now, the two quoter types are hardcoded here, it is an error to use a different one
-- TODO: this will change when quoters are added to "Analyzer.TypeDefinitions"
checkExpr (P.Quoter "json" s) = return $ JSON s
checkExpr (P.Quoter "psl" s) = return $ PSL s
checkExpr (P.Quoter tag _) = throw $ QuoterUnknownTag tag
-- Identifiers must have a type assigned to be used, so its type is checked in the bindings
checkExpr (P.Identifier ident) =
  lookupType ident >>= \case
    Nothing -> throw $ UndefinedIdentifier ident
    Just typ -> return $ Var ident typ
-- The type of a list is the unified type of its values.
-- This poses a problem for empty lists, there is not enough information to choose a type.
-- TODO: fix this in the future, probably by adding an additional phase to resolve type variables
--       that would be assigned here
checkExpr (P.List values) = do
  typedValues <- mapM checkExpr values
  case unify <$> nonEmpty typedValues of
    Nothing -> throw EmptyListNotImplemented
    Just (Left e) ->
      throw e
    Just (Right (unifiedValues, unifiedType)) ->
      return $ List (toList unifiedValues) (ListType unifiedType)
-- Apply [Dict], and also check that there are no duplicate keys in the dictionary
checkExpr (P.Dict entries) = do
  guardUnique $ map fst entries
  typedEntries <- zip (map fst entries) <$> mapM (checkExpr . snd) entries
  let dictType = M.fromList $ map (\(key, val) -> (key, DictRequired $ exprType val)) typedEntries
  return $ Dict typedEntries (DictType dictType)
  where
    guardUnique :: [String] -> T ()
    guardUnique [] = pure ()
    guardUnique (x : xs)
      | x `notElem` xs = guardUnique xs
      | otherwise = throw $ DictDuplicateField x

-- | @unify exprs == Right (exprs', typ)@ is a proof that for all @expr@ in @exprs@
-- "Γ ⊢ expr : typ". Additionally, the following property is guaranteed:
--
-- * @all ((==typ) . exprType) exprs == True@
--
-- A @Left@ return value is a proof that there is no @typ@ such that each @expr@
-- can be judged as that type.
unify :: NonEmpty TypedExpr -> Either TypeError (NonEmpty TypedExpr, Type)
unify (expr :| []) = Right (expr :| [], exprType expr)
unify (expr :| exprs) = do
  superType <- foldM unifyTypes (exprType expr) $ fmap exprType exprs
  fmap (,superType) $ mapM (weaken superType) $ expr :| exprs

-- | @unifyTypes s t@ finds the strongest type that both @s@ and @t@ are a sub-type of.
--
--   __Examples__
--
--   >>> unifyTypes StringType StringType
--   Right StringType
--
--   >>> unifyTypes (DictType $ M.empty) (DictType $ M.singleton "a" (DictRequired NumberType))
--   Right (DictType (M.singleton "a" (DictOptional NumberType)))
unifyTypes :: Type -> Type -> Either TypeError Type
-- Trivial case: two identical types unify to themselves
unifyTypes s t
  | s == t = Right s
-- Two lists unify only if their inner types unify
unifyTypes typS@(ListType s) typT@(ListType t) =
  fmap ListType $
    left (\e -> UnificationError (ReasonList e) typS typT) $
      unifyTypes s t
-- Declarations and enums can not unify with anything
unifyTypes s@(DeclType _) t = Left $ UnificationError ReasonDecl s t
unifyTypes s@(EnumType _) t = Left $ UnificationError ReasonEnum s t
-- The unification of two dictionaries is defined by the [DictNone] and [DictSome] rules
unifyTypes typS@(DictType s) typT@(DictType t) = do
  let keys = M.keysSet s <> M.keysSet t
  unifiedType <- foldMapM (\key -> M.singleton key <$> unifyEntryTypesForKey key) keys
  return $ DictType unifiedType
  where
    unifyEntryTypesForKey :: String -> Either TypeError DictEntryType
    unifyEntryTypesForKey key = annotateError key $ case (M.lookup key s, M.lookup key t) of
      (Nothing, Nothing) ->
        error "impossible: unifyTypes.unifyEntryTypesForKey should be called with only the keys of s and t"
      -- [DictSome] on s, [DictNone] on t
      (Just sType, Nothing) ->
        Right $ DictOptional $ dictEntryType sType
      -- [DictNone] on s, [DictSome] on t
      (Nothing, Just tType) ->
        Right $ DictOptional $ dictEntryType tType
      -- Both require @key@, so it must be a required entry of the unified entry types
      (Just (DictRequired sType), Just (DictRequired tType)) ->
        DictRequired <$> unifyTypes sType tType
      -- One of s or t has @key@ optionally, so it must be an optional entry of the unified entry types
      (Just sType, Just tType) ->
        DictOptional <$> unifyTypes (dictEntryType sType) (dictEntryType tType)

    annotateError :: String -> Either TypeError a -> Either TypeError a
    annotateError k = left (\e -> UnificationError (ReasonDictWrongKeyType k e) typS typT)
unifyTypes s t = Left $ UnificationError ReasonUncoercable s t

-- | @weaken expr typ@ attempts to weaken the type of @expr@ to @typ@. @weaken expr typ == Right expr'@
-- is a proof that "Γ ⊢ expr : typ". Additionally, the following property is guaranteed:
--
-- * @exprType expr == typ@
--
-- A @Left@ return value is a proof that "Γ ⊢ expr : typ" is false.
weaken :: Type -> TypedExpr -> Either TypeError TypedExpr
-- Trivial case: @expr@ already has type @typ@.
weaken typ expr
  | exprType expr == typ = Right expr
-- A list can be weakened to @typ@ if
-- - @typ@ is of the form @ListType typ'@
-- - Every value in the list can be weakened to @typ'@
weaken (ListType typ') expr@(List vals _) =
  fmap (\values -> List values (ListType typ')) $
    left (\e -> WeakenError (ReasonList e) expr typ') $
      mapM (weaken typ') vals
weaken (DictType typ') expr@(Dict entries _) = do
  entries' <- mapM weakenEntry entries
  mapM_ guardHasEntry $ M.toList typ'
  return $ Dict entries' $ DictType typ'
  where
    -- Tries to apply [DictSome] and [DictNone] rules to the entries of the dict
    weakenEntry :: (String, TypedExpr) -> Either TypeError (Ident, TypedExpr)
    weakenEntry (key, value) = case M.lookup key typ' of
      -- @key@ is missing from @typ'@ => extra keys are not allowed
      Nothing -> Left $ WeakenError (ReasonDictExtraKey key) expr (DictType typ')
      -- @key@ is required and present => only need to weaken the value's type
      Just (DictRequired valueTyp) -> (key,) <$> annotateError key (weaken valueTyp value)
      -- @key@ is optional and present => weaken value's type + use [DictSome]
      Just (DictOptional valueTyp) -> (key,) <$> annotateError key (weaken valueTyp value)

    -- Checks that all DictRequired entries in typ' exist in entries
    guardHasEntry :: (String, DictEntryType) -> Either TypeError ()
    guardHasEntry (key, DictOptional typ) = case lookup key entries of
      -- @key@ is optional and missing => use [DictNone]
      Nothing -> Right ()
      -- @key@ is optional and present => weaken the value's type + use [DictSome]
      Just entryVal -> case weaken typ entryVal of
        Left e -> Left $ WeakenError (ReasonDictWrongKeyType key e) expr (DictType typ')
        Right _ -> return ()
    guardHasEntry (k, DictRequired t) = case lookup k entries of
      -- @key@ is required and missing => not allowed
      Nothing -> Left $ WeakenError (ReasonDictNoKey k) expr (DictType typ')
      -- @key@ is required and present => only need to weaken value's type
      Just entryVal -> case weaken t entryVal of
        Left e -> Left $ WeakenError (ReasonDictWrongKeyType k e) expr (DictType typ')
        Right _ -> return ()

    -- Wraps a ReasonDictWrongKeyType error around a type error
    annotateError :: String -> Either TypeError a -> Either TypeError a
    annotateError k = left (\e -> WeakenError (ReasonDictWrongKeyType k e) expr (DictType typ'))
-- All other cases can not be weakened
weaken typ' expr = Left $ WeakenError ReasonUncoercable expr typ'

foldMapM :: (Foldable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
foldMapM f = foldl' (\ms a -> ms >>= \s -> (s <>) <$> f a) $ pure mempty
