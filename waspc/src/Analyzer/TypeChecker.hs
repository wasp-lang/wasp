{-# LANGUAGE LambdaCase #-}

module Analyzer.TypeChecker
  ( TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),
    TypeError (..),
    unify,
    checkExpr,
    exprType,
    typeCheck,
  )
where

import Analyzer.Parser (AST)
import qualified Analyzer.Parser as P
import Analyzer.Type
import Analyzer.TypeChecker.AST
  ( TypeError (..),
    TypedAST (..),
    TypedExpr (..),
    TypedStmt (..),
    exprType,
  )
import Analyzer.TypeChecker.Internal
import Analyzer.TypeDefinitions (TypeDefinitions)

-- | @isSuperType sup sub@ returns @True@ if @sup@ is a super type of @sub@.
--
--   ==== __Examples__
--
--   >>> isSuperType NumberType NumberType
--   True
--
--   >>> isSuperType NumberType StringType
--   False
--
--   >>> let maybe = DictType [DictOptionalEntry "a" NumberType]
--   >>> let none = DictType []
--   >>> let just = DictType [DictEntry "a" NumberType]
--   >>> (isSuperType maybe none, isSuperType maybe just)
--   (True, True)
isSuperType :: Type -> Type -> Bool
isSuperType sup sub
  | sup == sub = True
  | otherwise = case (sup, sub) of
    (DictType entries', DictType entries) -> False
    (ListType typ', ListType typ) -> isSuperType typ' typ

-- | @unify exprs@ will attempt to find the least general type @typ@ such that
--   every @expr@ in @exprs@ is a sub-type of @typ@. It returns a modified structure
--   where each typed expression is given the generalized type.
--
--   The structure @exprs@ must be non-empty.
unify :: (Traversable f) => f TypedExpr -> T (f TypedExpr)
unify exprs = do
  typ <- findLeastGeneralType exprs
  generalize typ exprs
  where
    findLeastGeneralType :: (Traversable f) => f TypedExpr -> T Type
    findLeastGeneralType = return . foldr1 (error "findLeastGeneralType not implemented") . fmap exprType

    generalize :: (Functor f) => Type -> f TypedExpr -> T (f TypedExpr)
    generalize sup = error "generalize not implemented"

-- | Create bindings for all declarations in the file to allow recursive
--   or out-of-lexical-order references.
hoistDeclarations :: AST -> T ()
hoistDeclarations (P.AST stmts) = mapM_ hoistDeclaration stmts
  where
    hoistDeclaration :: P.Stmt -> T ()
    -- Todo: check that typName is a real DeclType
    hoistDeclaration (P.Decl typName ident _) = setType ident $ DeclType typName

-- | Determine the type of an expression
checkExpr :: P.Expr -> T TypedExpr
checkExpr (P.StringLiteral s) = return $ StringLiteral s
checkExpr (P.IntegerLiteral i) = return $ IntegerLiteral i
checkExpr (P.DoubleLiteral d) = return $ DoubleLiteral d
checkExpr (P.BoolLiteral b) = return $ BoolLiteral b
checkExpr (P.ExtImport n s) = return $ ExtImport n s
checkExpr (P.Identifier ident) =
  lookupType ident >>= \case
    Nothing -> throw $ TypeError $ "Undefined identifier '" ++ ident ++ "'"
    Just typ -> return $ Var ident typ
checkExpr (P.Quoter "json" s) = return $ JSON s
checkExpr (P.Quoter "psl" s) = return $ PSL s
checkExpr (P.Quoter tag _) = throw $ TypeError $ "Unknown Quoter tag '" ++ tag ++ "'"
checkExpr (P.Dict entries) = error "dict type check unimplemented"
checkExpr (P.List entries) = error "list type check unimplemented"

-- | Checks that statements have valid types
checkStmt :: P.Stmt -> T TypedStmt
checkStmt (P.Decl typName _ expr) = error "check decl unimplemented"

checkAST :: AST -> T TypedAST
checkAST (P.AST stmts) = TypedAST <$> mapM checkStmt stmts

check :: AST -> T TypedAST
check ast = hoistDeclarations ast >> checkAST ast

-- | Checks that an AST conforms to the type rules of Wasp and produces a
--   an AST labelled with type information.
typeCheck :: TypeDefinitions -> AST -> Either TypeError TypedAST
typeCheck _ ast = runT $ check ast
