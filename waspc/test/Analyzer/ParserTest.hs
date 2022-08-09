module Analyzer.ParserTest where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Wasp.Analyzer.Parser hiding (withCtx)
import Wasp.Analyzer.Parser.ParseError (getErrorMessageAndCtx)
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion))
import Wasp.Util (indent)

test_Parser :: IO TestTree
test_Parser = do
  waspFiles <- findByExtension [".wasp"] "./test/Analyzer/parserTests"
  return $ testGroup "Wasp.Analyzer.Parser" $ map testCase waspFiles

testCase :: FilePath -> TestTree
testCase waspFile =
  let astFile = replaceExtension waspFile ".golden"
   in goldenVsStringDiff
        (takeBaseName waspFile) -- Test case name
        (\ref new -> ["diff", "-u", "--color=always", ref, new]) -- Diff command
        astFile -- Golden file path
        ( do
            -- Read from wasp file and return parse result
            source <- BSC.unpack <$> BS.readFile waspFile
            return $ BSC.pack $ showResult $ parseStatements source
        )

showResult :: Either ParseError AST -> String
showResult (Left err) =
  let (message, ctx) = getErrorMessageAndCtx err
      locationStr = "at " ++ showCtx ctx
   in "Parse error " ++ locationStr ++ indent 2 message
showResult (Right ast) = showAST ast

showAST :: AST -> String
showAST (AST stmts) = "(AST\n" ++ indent 2 (concatMap showStmt stmts) ++ ")\n"

showStmt :: WithCtx Stmt -> String
showStmt (WithCtx ctx (Decl typ name body)) =
  withCtx "(Decl" ctx ++ " type=" ++ typ ++ " name=" ++ name ++ "\n"
    ++ indent 2 (showExpr body)
    ++ ")\n"

showExpr :: WithCtx Expr -> String
showExpr (WithCtx ctx expr) = "(" ++ withCtx (exprName expr) ctx ++ showDetails expr ++ ")\n"
  where
    exprName (Dict _) = "Dict"
    exprName (List _) = "List"
    exprName (Tuple _) = "List"
    exprName (StringLiteral _) = "String"
    exprName (IntegerLiteral _) = "Integer"
    exprName (DoubleLiteral _) = "Double"
    exprName (BoolLiteral _) = "Bool"
    exprName (ExtImport _ _) = "ExtImport"
    exprName (Var _) = "Var"
    exprName (Quoter _ _) = "Quoter"

    showDetails (Dict entries) = "\n" ++ indent 2 (concatMap showEntry entries)
    showDetails (List values) = "\n" ++ indent 2 (concatMap showExpr values)
    showDetails (Tuple (a, b, cs)) = "\n" ++ indent 2 (concatMap showExpr (a : b : cs))
    showDetails (StringLiteral s) = showLiteral s
    showDetails (IntegerLiteral n) = showLiteral n
    showDetails (DoubleLiteral n) = showLiteral n
    showDetails (BoolLiteral b) = showLiteral b
    showDetails (ExtImport name path) = " " ++ showExtImportName name ++ " path=" ++ show path ++ "\n"
    showDetails (Var v) = " variable=" ++ v
    showDetails (Quoter tag contents) = " tag=" ++ tag ++ "\n" ++ indent 2 contents

    showEntry (key, value) = "(DictEntry key=" ++ key ++ "\n" ++ indent 2 (showExpr value) ++ ")\n"

    showExtImportName (ExtImportField name) = "field=" ++ name
    showExtImportName (ExtImportModule name) = "module=" ++ name

    showLiteral :: Show a => a -> String
    showLiteral value = " value=" ++ show value

withCtx :: String -> Ctx -> String
withCtx name ctx = name ++ "@" ++ showCtx ctx

showCtx :: Ctx -> String
showCtx (Ctx (SourceRegion (SourcePosition sl sc) (SourcePosition el ec)))
  | sl == el && sc == ec = show sl ++ ":" ++ show sc
  | sl == el = show sl ++ ":" ++ show sc ++ "-" ++ show ec
  | otherwise = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
