module Wasp.Analyzer.Parser.AST.PrettyPrinter
  ( prettyPrintAST,
    prettyPrintStmt,
    prettyPrintExpr,
    prettyPrintCtx,
    prettyPrintWithCtx,
  )
where

import Wasp.Analyzer.Parser hiding (withCtx)
import Wasp.Util (indent)

prettyPrintAST :: AST -> String
prettyPrintAST (AST stmts) = "(AST\n" ++ indent 2 (concatMap prettyPrintStmt stmts) ++ ")\n"

prettyPrintStmt :: WithCtx Stmt -> String
prettyPrintStmt (WithCtx ctx (Decl typ name body)) =
  prettyPrintWithCtx "(Decl" ctx ++ " type=" ++ typ ++ " name=" ++ name ++ "\n"
    ++ indent 2 (prettyPrintExpr body)
    ++ ")\n"

prettyPrintExpr :: WithCtx Expr -> String
prettyPrintExpr (WithCtx ctx expr) = "(" ++ prettyPrintWithCtx (exprName expr) ctx ++ showDetails expr ++ ")\n"
  where
    exprName (Dict _) = "Dict"
    exprName (List _) = "List"
    exprName (Tuple _) = "Tuple"
    exprName (StringLiteral _) = "String"
    exprName (IntegerLiteral _) = "Integer"
    exprName (DoubleLiteral _) = "Double"
    exprName (BoolLiteral _) = "Bool"
    exprName (ExtImport _ _) = "ExtImport"
    exprName (Var _) = "Var"
    exprName (Quoter _ _) = "Quoter"

    showDetails (Dict []) = ""
    showDetails (Dict entries) = "\n" ++ indent 2 (concatMap showEntry entries)
    showDetails (List []) = ""
    showDetails (List values) = "\n" ++ indent 2 (concatMap prettyPrintExpr values)
    showDetails (Tuple (a, b, cs)) = "\n" ++ indent 2 (concatMap prettyPrintExpr (a : b : cs))
    showDetails (StringLiteral s) = showLiteral s
    showDetails (IntegerLiteral n) = showLiteral n
    showDetails (DoubleLiteral n) = showLiteral n
    showDetails (BoolLiteral b) = showLiteral b
    showDetails (ExtImport name path) = " " ++ showExtImportName name ++ " path=" ++ show path
    showDetails (Var v) = " variable=" ++ v
    showDetails (Quoter tag contents) = " tag=" ++ tag ++ "\n" ++ indent 2 ("{=" ++ contents ++ "=}") ++ "\n"

    showEntry (key, value) = "(DictEntry key=" ++ key ++ "\n" ++ indent 2 (prettyPrintExpr value) ++ ")\n"

    showExtImportName (ExtImportField name) = "field=" ++ name
    showExtImportName (ExtImportModule name) = "module=" ++ name

    showLiteral :: Show a => a -> String
    showLiteral value = " value=" ++ show value

prettyPrintWithCtx :: String -> Ctx -> String
prettyPrintWithCtx name ctx = name ++ "@" ++ prettyPrintCtx ctx

prettyPrintCtx :: Ctx -> String
prettyPrintCtx (Ctx (SourceRegion (SourcePosition sl sc) (SourcePosition el ec)))
  | sl == el && sc == ec = show sl ++ ":" ++ show sc
  | sl == el = show sl ++ ":" ++ show sc ++ "-" ++ show ec
  | otherwise = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
