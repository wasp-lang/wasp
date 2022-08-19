module Wasp.Analyzer.Parser.PrettyPrinter
  ( prettyPrintParserResult,
  )
where

import Wasp.Analyzer.Parser hiding (withCtx)
import Wasp.Analyzer.Parser.AST.PrettyPrinter (prettyPrintAST, prettyPrintCtx)
import Wasp.Analyzer.Parser.ParseError (getErrorMessageAndCtx)
import Wasp.Util (indent)

prettyPrintParserResult :: Either ParseError AST -> String
prettyPrintParserResult (Left err) =
  let (message, ctx) = getErrorMessageAndCtx err
      locationStr = "at " ++ prettyPrintCtx ctx
   in "Parse error " ++ locationStr ++ ":\n" ++ indent 2 message ++ "\n"
prettyPrintParserResult (Right ast) = prettyPrintAST ast
