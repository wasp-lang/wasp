{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | This module implements the type rules defined by the wasplang document
-- in two phases.
--
-- = Hoisting Declarations
--
-- First, the variables bound by each declaration statement, e.g.
--
-- @
-- app Todo {
--   title: "Todo App"
-- }
-- @
--
-- are created, without checking any types. In the above example, the bindings
-- created would be @[("Todo", DeclType "app")]@.
--
-- = Statement Type Checking
--
-- In this second phase, the types of the argument to each declaration are checked
-- to ensure they are valid for the declaration type. The implementation of the
-- type inference rules is in "inferExprType", "unifyTypes", and "weaken".
module Wasp.Analyzer.TypeChecker.Internal
  ( check,
    hoistDeclarations,
    checkAST,
    checkStmt,
    inferExprType,
    unify,
    unifyTypes,
    weaken,
  )
where

import Control.Arrow (left, second)
import Control.Monad (foldM, void)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Wasp.Analyzer.Parser (AST)
import qualified Wasp.Analyzer.Parser as P
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.AST
import Wasp.Analyzer.TypeChecker.Monad
import Wasp.Analyzer.TypeChecker.TypeError
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.Util.Control.Monad (foldM1, foldMapM')

check :: AST -> TypeChecker TypedAST
check ast = hoistDeclarations ast >> checkAST ast

hoistDeclarations :: AST -> TypeChecker ()
hoistDeclarations (P.AST stmts) = mapM_ hoistDeclaration stmts
  where
    hoistDeclaration :: P.Stmt -> TypeChecker ()
    hoistDeclaration (P.Decl typeName ident _) = setType ident $ DeclType typeName

checkAST :: AST -> TypeChecker TypedAST
checkAST (P.AST stmts) = TypedAST <$> mapM checkStmt stmts

checkStmt :: P.Stmt -> TypeChecker TypedStmt
checkStmt (P.Decl typeName name expr) =
  lookupDeclType typeName >>= \case
    Nothing -> throw $ NoDeclarationType typeName
    Just (TD.DeclType _ expectedType _) -> do
      -- Decides whether the argument to the declaration has the correct type
      mTypedExpr <- weaken expectedType <$> inferExprType expr
      case mTypedExpr of
        Left e -> throw e
        Right typedExpr -> return $ Decl name typedExpr (DeclType typeName)

-- | Determine the type of an expression, following the inference rules described in
-- the wasplang document. Some these rules are referenced by name in the comments
-- of the following functions using [Brackets].
inferExprType :: P.Expr -> TypeChecker TypedExpr
inferExprType (P.StringLiteral s) = return $ StringLiteral s
inferExprType (P.IntegerLiteral i) = return $ IntegerLiteral i
inferExprType (P.DoubleLiteral d) = return $ DoubleLiteral d
inferExprType (P.BoolLiteral b) = return $ BoolLiteral b
inferExprType (P.ExtImport n s) = return $ ExtImport n s
inferExprType (P.Var ident) =
  lookupType ident >>= \case
    Nothing -> throw $ UndefinedIdentifier ident
    Just typ -> return $ Var ident typ
-- For now, the two quoter types are hardcoded here, it is an error to use a different one
-- TODO: this will change when quoters are added to "Analyzer.TypeDefinitions"
inferExprType (P.Quoter "json" s) = return $ JSON s
inferExprType (P.Quoter "psl" s) = return $ PSL s
inferExprType (P.Quoter tag _) = throw $ QuoterUnknownTag tag
-- The type of a list is the unified type of its values.
-- This poses a problem for empty lists, there is not enough information to choose a type.
-- TODO: fix this in the future, probably by adding an additional phase to resolve type variables
--       that would be assigned here
inferExprType (P.List values) = do
  typedValues <- mapM inferExprType values
  case unify <$> nonEmpty typedValues of
    -- Apply [EmptyList]
    Nothing -> return $ List [] EmptyListType
    Just (Left e) ->
      throw e
    Just (Right (unifiedValues, unifiedType)) ->
      return $ List (toList unifiedValues) (ListType unifiedType)
-- Apply [Dict], and also check that there are no duplicate keys in the dictionary
inferExprType (P.Dict entries) = do
  typedEntries <- mapM (\(k, v) -> (k,) <$> inferExprType v) entries
  dictType <-
    foldM insertIfUniqueElseThrow M.empty $
      second (DictRequired . exprType) <$> typedEntries
  return $ Dict typedEntries (DictType dictType)
  where
    insertIfUniqueElseThrow :: M.HashMap Identifier v -> (Identifier, v) -> TypeChecker (M.HashMap Identifier v)
    insertIfUniqueElseThrow m (key, value)
      | key `M.member` m = throw $ DictDuplicateField key
      | otherwise = return $ M.insert key value m
inferExprType (P.Tuple (value1, value2, restOfValues)) = do
  typedValue1 <- inferExprType value1
  typedValue2 <- inferExprType value2
  typedRestOfValues <- mapM inferExprType restOfValues
  let typedValues = (typedValue1, typedValue2, typedRestOfValues)
  let tupleType =
        TupleType
          ( exprType typedValue1,
            exprType typedValue2,
            exprType <$> typedRestOfValues
          )
  return $ Tuple typedValues tupleType

-- | Finds the strongest common type for all of the given expressions, "common" meaning
-- all the expressions can be typed with it and "strongest" meaning it is as specific
-- as possible. If such a type exists, it returns that type and all of the given expressions
-- typed with the new type. If no such type exists, it returns an error.
--
-- The following property is gauranteed:
--
-- * If @unify exprs == Right (exprs', commonType)@ then @all ((==commonType) . exprType) exprs'@
--
-- __Examples__
--
-- >>> unify (StringLiteral "a" :| DoubleLiteral 6.28)
-- Left $ UnifyError ReasonUncoercable StringType NumberType
--
-- >>> unify (Dict [("a", IntegerLiteral 2) _ :| Dict [] _)
-- Right (Dict [("a", IntegerLiteral 2)] _ :| Dict [] _, DictType (M.singleton "a" (DictOptional NumberType)))
unify :: NonEmpty TypedExpr -> Either TypeError (NonEmpty TypedExpr, Type)
unify exprs = do
  superType <- foldM1 unifyTypes (exprType <$> exprs)
  (,superType) <$> mapM (weaken superType) exprs

-- unify (expr :| []) = Right (expr :| [], exprType expr)
-- unify (expr :| exprs) = do
--   superType <- foldM unifyTypes (exprType expr) $ fmap exprType exprs
--   fmap (,superType) $ mapM (weaken superType) $ expr :| exprs

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
unifyTypes type1 type2
  | type1 == type2 = Right type1
-- Apply [AnyList]: an empty list can unify with any other list
unifyTypes EmptyListType typ@(ListType _) = Right typ
unifyTypes typ@(ListType _) EmptyListType = Right typ
-- Two non-empty lists unify only if their inner types unify
unifyTypes type1@(ListType elemType1) type2@(ListType elemType2) =
  annotateError $ ListType <$> unifyTypes elemType1 elemType2
  where
    annotateError = left (\e -> UnificationError (ReasonList e) type1 type2)
-- Declarations and enums can not unify with anything
unifyTypes type1@(DeclType _) type2 = Left $ UnificationError ReasonDecl type1 type2
unifyTypes type1@(EnumType _) type2 = Left $ UnificationError ReasonEnum type1 type2
-- The unification of two dictionaries is defined by the [DictNone] and [DictSome] rules
unifyTypes type1@(DictType entryTypes1) type2@(DictType entryTypes2) = do
  let keys = M.keysSet entryTypes1 <> M.keysSet entryTypes2
  unifiedType <- foldMapM' (\key -> M.singleton key <$> unifyEntryTypesForKey key) keys
  return $ DictType unifiedType
  where
    unifyEntryTypesForKey :: String -> Either TypeError DictEntryType
    unifyEntryTypesForKey key = annotateError key $ case (M.lookup key entryTypes1, M.lookup key entryTypes2) of
      (Nothing, Nothing) ->
        error "impossible: unifyTypes.unifyEntryTypesForKey should be called with only the keys of entryTypes1 and entryTypes2"
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
    annotateError k = left (\e -> UnificationError (ReasonDictWrongKeyType k e) type1 type2)
unifyTypes type1 type2 = Left $ UnificationError ReasonUncoercable type1 type2

-- | Converts a typed expression from its current type to the given weaker type, "weaker"
-- meaning it is a super-type of the original type. If that is possible, it returns the
-- converted expression. If not, an error is returned.
--
-- The following property is guaranteed:
--
-- * If @weaken typ expr == Right expr'@ then @exprType expr' == typ@
--
-- When a @Left@ value is returned, then @expr@ can not be typed as @typ@.
weaken :: Type -> TypedExpr -> Either TypeError TypedExpr
weaken type' expr
  | exprType expr == type' = Right expr
-- Apply [AnyList]: An empty list can be weakened to any list type
weaken type'@(ListType _) (List [] EmptyListType) = return $ List [] type'
-- A non-empty list can be weakened to @typ@ if
-- - @typ@ is of the form @ListType type'@
-- - Every value in the list can be weakened to @type'@
weaken type'@(ListType elemType') expr@(List elems _) = do
  elems' <- annotateError $ mapM (weaken elemType') elems
  return $ List elems' type'
  where
    annotateError = left (\e -> WeakenError (ReasonList e) expr elemType')
weaken (DictType entryTypes') expr@(Dict entries _) = do
  entries' <- mapM weakenEntry entries
  mapM_ ensureExprSatisifiesEntryType $ M.toList entryTypes'
  return $ Dict entries' $ DictType entryTypes'
  where
    -- Tries to apply [DictSome] and [DictNone] rules to the entries of the dict
    weakenEntry :: (String, TypedExpr) -> Either TypeError (Identifier, TypedExpr)
    weakenEntry (key, value) = case M.lookup key entryTypes' of
      -- @key@ is missing from @typ'@ => extra keys are not allowed
      Nothing -> Left $ WeakenError (ReasonDictExtraKey key) expr (DictType entryTypes')
      -- @key@ is required and present => only need to weaken the value's type
      Just (DictRequired valueTyp) -> (key,) <$> annotateError key (weaken valueTyp value)
      -- @key@ is optional and present => weaken value's type + use [DictSome]
      Just (DictOptional valueTyp) -> (key,) <$> annotateError key (weaken valueTyp value)

    -- Checks that all DictRequired entries in typ' exist in entries
    ensureExprSatisifiesEntryType :: (String, DictEntryType) -> Either TypeError ()
    ensureExprSatisifiesEntryType (key, DictOptional typ) = case lookup key entries of
      -- @key@ is optional and missing => use [DictNone]
      Nothing -> Right ()
      -- @key@ is optional and present => weaken the value's type + use [DictSome]
      Just entryVal -> void $ annotateError key $ weaken typ entryVal
    ensureExprSatisifiesEntryType (key, DictRequired typ) = case lookup key entries of
      -- @key@ is required and missing => not allowed
      Nothing -> Left $ WeakenError (ReasonDictNoKey key) expr (DictType entryTypes')
      -- @key@ is required and present => only need to weaken value's type
      Just entryVal -> void $ annotateError key $ weaken typ entryVal

    -- Wraps a ReasonDictWrongKeyType error around a type error
    annotateError :: String -> Either TypeError a -> Either TypeError a
    annotateError k = left (\e -> WeakenError (ReasonDictWrongKeyType k e) expr (DictType entryTypes'))
-- All other cases can not be weakened
weaken typ' expr = Left $ WeakenError ReasonUncoercable expr typ'
