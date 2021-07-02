{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.TypeChecker
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    TypeError (..),
    TypeCoerceReason (..),
    unify,
    unifyTypes,
    checkExpr,
    checkStmt,
    exprType,
    typeCheck,
  )
where

import Analyzer.Parser (AST)
import qualified Analyzer.Parser as P
import Analyzer.Type
  ( DictEntryType (DictOptional, DictRequired),
    Type (DeclType, DictType, EnumType, ListType),
  )
import Analyzer.TypeChecker.AST
  ( TypeCoerceReason (..),
    TypeError (..),
    TypedAST (..),
    TypedExpr (..),
    TypedStmt (..),
    exprType,
  )
import Analyzer.TypeChecker.Internal
import Analyzer.TypeDefinitions (TypeDefinitions)
import qualified Analyzer.TypeDefinitions as TD
import Control.Arrow (left)
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as H

-- | Fold map over the keys and values of a hash map, using a monoid embedded in a monad
--   to collect the values.
foldMapMWithKey :: (Monad m, Monoid s) => (k -> v -> m s) -> H.HashMap k v -> m s
foldMapMWithKey f = H.foldlWithKey' (\m k v -> m >>= \s -> (s <>) <$> f k v) $ return mempty

-- | @weaken expr typ@ attempts to weaken the type of @expr@ to @typ@, if
--   @typ@ is a super type of the type of @expr@.
weaken :: Type -> TypedExpr -> Either TypeError TypedExpr
weaken typ expr
  | exprType expr == typ = Right expr
weaken typ' expr@(List vals _) =
  fmap (flip List typ') $
    left (\e -> WeakenError (ReasonList e) expr typ') $
      mapM (weaken typ') vals
weaken (DictType typ') expr@(Dict entries _) = do
  entries' <- mapM go entries
  mapM_ guardHasEntry $ H.toList typ'
  return $ Dict entries' $ DictType typ'
  where
    -- Tries to apply DictSome and DictNone rules to the entries of the dict
    go :: (String, TypedExpr) -> Either TypeError (P.Ident, TypedExpr)
    go (k, s) = case H.lookup k typ' of
      -- expr has an extra key, so typ' is not more general
      Nothing -> Left $ WeakenError (ReasonDictExtraKey k) expr (DictType typ')
      -- No rules applied to expr
      Just (DictRequired t) -> (k,) <$> annotateError k (weaken t s)
      -- DictSome applied to expr
      Just (DictOptional t) -> (k,) <$> annotateError k (weaken t s)

    -- Checks that all DictRequired entries in typ' exist in entries
    guardHasEntry :: (String, DictEntryType) -> Either TypeError ()
    guardHasEntry (k, DictOptional t) = case lookup k entries of
      -- DictNone applied to expr
      Nothing -> Right ()
      -- DictSome applied to expr
      Just entryVal -> case weaken t entryVal of
        Left e -> Left $ WeakenError (ReasonDictWrongKeyType k e) expr (DictType typ')
        Right _ -> return ()
    guardHasEntry (k, DictRequired t) = case lookup k entries of
      -- entries is missing a required key
      Nothing -> Left $ WeakenError (ReasonDictNoKey k) expr (DictType typ')
      Just entryVal -> case weaken t entryVal of
        Left e -> Left $ WeakenError (ReasonDictWrongKeyType k e) expr (DictType typ')
        Right _ -> return ()

    -- Wraps a ReasonDictWrongKeyType error around a type error
    annotateError :: String -> Either TypeError a -> Either TypeError a
    annotateError k = left (\e -> WeakenError (ReasonDictWrongKeyType k e) expr (DictType typ'))
weaken typ' expr = Left $ WeakenError ReasonUncoercable expr typ'

-- | @unifyTypes s t@ find the strongest type that both s and t can be typed to.
--
--   ==== __Examples__
--
--   >>> unifyTypes StringType StringType
--   Right StringType
--
--   >>> unifyTypes (DictType $ H.empty) (DictType $ H.singleton "a" (DictRequired NumberType))
--   Right (DictType (H.singleton "a" (DictOptional NumberType)))
unifyTypes :: Type -> Type -> Either TypeError Type
unifyTypes s t
  | s == t = Right s
unifyTypes typS@(ListType s) typT@(ListType t) =
  fmap ListType $
    left (\e -> UnificationError (ReasonList e) typS typT) $
      unifyTypes s t
unifyTypes s@(DeclType _) t = Left $ UnificationError ReasonDecl s t
unifyTypes s@(EnumType _) t = Left $ UnificationError ReasonEnum s t
unifyTypes typS@(DictType s) typT@(DictType t) = do
  -- Rules are applied in both directions, then unioned because s may not
  -- have keys that t does, or vice versa
  -- TODO: should this be improved?
  onS <- foldMapMWithKey (go t) s
  onT <- foldMapMWithKey (go s) t
  return $ DictType $ onS <> onT
  where
    -- Tries to apply DictSome and DictNone rules to s and u
    go :: H.HashMap String DictEntryType -> String -> DictEntryType -> Either TypeError (H.HashMap String DictEntryType)
    go u k (DictRequired s') = annotateError k $ case H.lookup k u of
      -- DictSome on s, DictNone on u
      Nothing -> Right $ H.singleton k (DictOptional s')
      -- No rules applied to s or u
      Just (DictRequired u') -> H.singleton k . DictRequired <$> unifyTypes s' u'
      -- DictNone on s
      Just (DictOptional u') -> H.singleton k . DictOptional <$> unifyTypes s' u'
    go u k (DictOptional s') = annotateError k $ case H.lookup k u of
      -- DictNone on u
      Nothing -> Right $ H.singleton k (DictOptional s')
      -- DictSome on u
      Just (DictRequired u') -> H.singleton k . DictOptional <$> unifyTypes s' u'
      -- No rules applied to s or u
      Just (DictOptional u') -> H.singleton k . DictOptional <$> unifyTypes s' u'

    annotateError :: String -> Either TypeError a -> Either TypeError a
    annotateError k = left (\e -> UnificationError (ReasonDictWrongKeyType k e) typS typT)
unifyTypes s t = Left $ UnificationError ReasonUncoercable s t

-- | @unify exprs@ tries to weaken the types of the expressions in @expr@ so
--   they all have the same type.
unify :: [TypedExpr] -> Either TypeError [TypedExpr]
unify [] = Right []
unify (expr : exprs) = do
  sup <- foldM unifyTypes (exprType expr) $ fmap exprType exprs
  mapM (weaken sup) $ expr : exprs

-- | Create bindings for all declarations in the file to allow recursive
--   or out-of-lexical-order references.
hoistDeclarations :: AST -> T ()
hoistDeclarations (P.AST stmts) = mapM_ hoistDeclaration stmts
  where
    hoistDeclaration :: P.Stmt -> T ()
    -- Todo: check that typName is a real DeclType
    hoistDeclaration (P.Decl typName ident _) =
      lookupDecl typName >>= \case
        Nothing -> throw $ NoDeclarationType typName
        Just _ -> setType ident $ DeclType typName

-- | Determine the type of an expression
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
checkExpr (P.Quoter "json" s) = return $ JSON s
checkExpr (P.Quoter "psl" s) = return $ PSL s
checkExpr (P.Quoter tag _) = throw $ QuoterUnknownTag tag
checkExpr (P.Dict entries) = do
  guardUnique $ map fst entries
  typedEntries <- zip (map fst entries) <$> mapM (checkExpr . snd) entries
  let dictType = H.fromList $ map (\(key, val) -> (key, DictRequired $ exprType val)) typedEntries
  return $ Dict typedEntries (DictType dictType)
  where
    guardUnique :: [String] -> T ()
    guardUnique [] = pure ()
    guardUnique (x : xs)
      | x `notElem` xs = guardUnique xs
      | otherwise = throw $ DictDuplicateField x
checkExpr (P.List values) = do
  typedValues <- mapM checkExpr values
  let unifiedValues = unify typedValues
  case unifiedValues of
    Left e -> throw e
    -- TODO: type check empty list. this will require type inference...
    Right [] -> throw EmptyListNotImplemented
    Right xs@(x : _) -> return $ List xs (ListType $ exprType x)

-- | Checks that statements have valid types
checkStmt :: P.Stmt -> T TypedStmt
checkStmt (P.Decl typName name expr) =
  lookupDecl typName >>= \case
    Nothing -> throw $ NoDeclarationType typName
    Just (TD.DeclType _ expectedType) -> do
      mTypedExpr <- weaken expectedType <$> checkExpr expr
      case mTypedExpr of
        Left e -> throw e
        Right typedExpr -> return $ Decl name typedExpr (DeclType typName)

checkAST :: AST -> T TypedAST
checkAST (P.AST stmts) = TypedAST <$> mapM checkStmt stmts

check :: AST -> T TypedAST
check ast = hoistDeclarations ast >> checkAST ast

-- | Checks that an AST conforms to the type rules of Wasp and produces a
--   an AST labelled with type information.
typeCheck :: TypeDefinitions -> AST -> Either TypeError TypedAST
typeCheck tds ast = runT tds $ check ast
