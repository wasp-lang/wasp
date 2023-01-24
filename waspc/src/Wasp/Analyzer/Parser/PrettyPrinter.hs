module Wasp.Analyzer.Parser.PrettyPrinter
  ( prettyPrintParserResult,
  )
where

import Wasp.Analyzer.Parser.AST (AST)
import Wasp.Analyzer.Parser.AST.PrettyPrinter (prettyPrintAST, prettyPrintCtx)
import Wasp.Analyzer.Parser.ParseError (ParseError, getErrorMessageAndCtx)
import Wasp.Util (indent)

prettyPrintParserResult :: Either ParseError AST -> String
prettyPrintParserResult (Right ast) = prettyPrintAST ast
prettyPrintParserResult (Left err) = "Parse error at " ++ locationStr ++ ":\n" ++ indent 2 message ++ "\n"
  where
    (message, ctx) = getErrorMessageAndCtx err
    locationStr = prettyPrintCtx ctx
