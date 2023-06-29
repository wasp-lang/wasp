module Wasp.Analyzer.Parser.PrettyPrinter
  ( prettyPrintParserResult,
  )
where

import Data.List (intercalate)
import Wasp.Analyzer.Parser.AST (AST)
import Wasp.Analyzer.Parser.AST.PrettyPrinter (prettyPrintAST, prettyPrintCtx)
import Wasp.Analyzer.Parser.ParseError (ParseError, getErrorMessageAndCtx)
import Wasp.Util (indent)

prettyPrintParserResult :: Either [ParseError] AST -> String
prettyPrintParserResult (Right ast) = prettyPrintAST ast
prettyPrintParserResult (Left errs) = intercalate "\n" $ map prettyPrintErr errs
  where
    prettyPrintErr err =
      let (message, ctx) = getErrorMessageAndCtx err
          locationStr = prettyPrintCtx ctx
       in "Parse error at " ++ locationStr ++ ":\n" ++ indent 2 message ++ "\n"
