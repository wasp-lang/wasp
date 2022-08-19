module Analyzer.ParserTest where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser hiding (withCtx)
import Wasp.Analyzer.Parser.ParseError (getErrorMessageAndCtx)
import Wasp.Util (indent)

spec_IsValidWaspIdentifier :: Spec
spec_IsValidWaspIdentifier = do
  it "Correctly identifies valid identifiers" $ do
    all isValidWaspIdentifier ["foo", "foo12", "_12", "_"]
  it "Correctly identifies invalid identifiers" $ do
    not (any isValidWaspIdentifier ["true", "12", "12_", "1foo"])

spec_ParseExpression :: Spec
spec_ParseExpression = do
  it "Parses identifiers" $ do
    parseExpression "foo" `shouldBe` Right (Var "foo")
    parseExpression "foo12" `shouldBe` Right (Var "foo12")
    parseExpression "_" `shouldBe` Right (Var "_")
    parseExpression "_12" `shouldBe` Right (Var "_12")
  it "Parses double literals" $ do
    parseExpression "0.5" `shouldBe` Right (DoubleLiteral 0.5)
  it "Parses int literals" $ do
    parseExpression "5" `shouldBe` Right (IntegerLiteral 5)

test_ParseStatements :: IO TestTree
test_ParseStatements = do
  -- To make writing tests easier, we use golden files to define our tests here.
  --
  -- In parseStatementsTests/ dir there are .wasp files and .golden files, where
  -- each .wasp source file represents input of a single test,
  -- while corresponding .golden file represents expected output of that test.
  --
  -- See `declsDictsAndLiterals` for an example of a test case with a successful
  -- parse.
  --
  -- See `dictNoCloseBracket` for an example of a test case with an unsuccessful
  -- parse.
  --
  -- While the testing framework will create the `.golden` file for you if it does
  -- not exist, it is recommended that you manually write the `.golden` file to
  -- make sure the output is as expected.
  --
  -- When the golden file does not match the actual output, a diff will be shown
  -- in the terminal.
  waspFiles <- findByExtension [".wasp"] "./test/Analyzer/ParserTest/parseStatementsTests"
  return $
    testGroup "Wasp.Analyzer.Parser.parseStatements" $
      makeParseStatementsGoldenTest <$> waspFiles

-- | Make a golden test where given wasp source file is parsed with
-- @parseStatements@ and the AST that is the result of parsing is the output of test.
-- This means that AST will be persisted to codebase as a "golden" output and compared
-- against the next time this test is run -> any difference is reported as failed test.
-- If no such file exists yet on the disk, it is created.
makeParseStatementsGoldenTest :: FilePath -> TestTree
makeParseStatementsGoldenTest waspFile =
  let goldenAstFile = replaceExtension waspFile ".golden"
      testCaseName = takeBaseName waspFile
      diffCmd = \ref new -> ["diff", "-u", ref, new]
   in goldenVsStringDiff
        testCaseName
        diffCmd
        goldenAstFile
        ( do
            -- Read from wasp file and return parse result.
            source <- BSC.unpack <$> BS.readFile waspFile
            return $ BSC.pack $ prettyPrintParserResult $ parseStatements source
        )

-- | Pretty print the result of a parse. The purpose of a custom implementation
-- here is to make golden file diffs readable/useful.
--
-- To see examples of pretty prints, see the golden files in `./ParserTest/parseStatementsTests`.
prettyPrintParserResult :: Either ParseError AST -> String
prettyPrintParserResult (Left err) =
  let (message, ctx) = getErrorMessageAndCtx err
      locationStr = "at " ++ prettyPrintCtx ctx
   in "Parse error " ++ locationStr ++ ":\n" ++ indent 2 message ++ "\n"
prettyPrintParserResult (Right ast) = prettyPrintAST ast

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
