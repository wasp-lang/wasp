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
-- type inference rules is in "checkExpr", "unifyTypes", and "weaken".
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
    hoistDeclaration (P.Decl typName ident _) = setType ident $ DeclType typName

checkAST :: AST -> T TypedAST
checkAST (P.AST stmts) = TypedAST <$> mapM checkStmt stmts

checkStmt :: P.Stmt -> T TypedStmt
checkStmt (P.Decl typName name expr) =
  lookupDeclType typName >>= \case
    Nothing -> throw $ NoDeclarationType typName
    Just (TD.DeclType _ expectedType) -> do
      -- Decides whether the argument to the declaration has the correct type
      mTypedExpr <- weaken expectedType <$> checkExpr expr
      case mTypedExpr of
        Left e -> throw e
        Right typedExpr -> return $ Decl name typedExpr (DeclType typName)

-- | Determine the type of an expression, following the inference rules described in
-- the wasplang document. Some these rules are referenced by name in the comments
-- of the following functions using [Brackets].
checkExpr :: P.Expr -> T TypedExpr
checkExpr (P.StringLiteral s) = return $ StringLiteral s
checkExpr (P.IntegerLiteral i) = return $ IntegerLiteral i
checkExpr (P.DoubleLiteral d) = return $ DoubleLiteral d
checkExpr (P.BoolLiteral b) = return $ BoolLiteral b
checkExpr (P.ExtImport n s) = return $ ExtImport n s
checkExpr (P.Identifier ident) =
  lookupType ident >>= \case
    Nothing -> throw $ UndefinedIdentifier ident
    Just typ -> return $ Var ident typ
-- For now, the two quoter types are hardcoded here, it is an error to use a different one
-- TODO: this will change when quoters are added to "Analyzer.TypeDefinitions"
checkExpr (P.Quoter "json" s) = return $ JSON s
checkExpr (P.Quoter "psl" s) = return $ PSL s
checkExpr (P.Quoter tag _) = throw $ QuoterUnknownTag tag
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

-- | @unify exprs == Right (exprs', typ)@ shows that all expressions in @exprs@
-- can be typed as @typ@. Additionally, The following property is guaranteed:
--
-- * @all ((==typ) . exprType) exprs == True@
--
-- When a @Left@ value is returned, then there is no @typ@ that all expressions in
-- @expr@ can be typed as.
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
  unifiedType <- foldMapM' (\key -> M.singleton key <$> unifyEntryTypesForKey key) keys
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

-- | @weaken expr typ == Right expr'@ establishes that @expr@ can be typed as @typ@.
-- Additionally, the following property is guaranteed:
--
-- * @exprType expr == typ@
--
-- When a @Left@ value is returned, then @expr@ can not be typed as @typ@.
weaken :: Type -> TypedExpr -> Either TypeError TypedExpr
weaken typ' expr
  | exprType expr == typ' = Right expr
-- A list can be weakened to @typ@ if
-- - @typ@ is of the form @ListType typ'@
-- - Every value in the list can be weakened to @typ'@
weaken type'@(ListType elemType') expr@(List elems _) = do
  elems' <- left (\e -> WeakenError (ReasonList e) expr elemType') $
    mapM (weaken elemType') elems
  return $ List elems' type'
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

-- | "Prelude.foldMap'" (using a strict left fold), but in a monad "m".
foldMapM' :: (Foldable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
foldMapM' f = foldl' (\ms a -> ms >>= \s -> (s <>) <$> f a) $ pure mempty
