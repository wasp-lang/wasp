module Wasp.Psl.Generator.Argument where

import Data.List (intercalate)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import Wasp.Psl.Generator.Common (PslSource)

generateArgument :: Psl.Argument.Argument -> PslSource
generateArgument (Psl.Argument.ArgNamed name value) = name ++ ": " ++ generateExpression value
generateArgument (Psl.Argument.ArgUnnamed value) = generateExpression value

generateExpression :: Psl.Argument.Expression -> PslSource
generateExpression = \case
  Psl.Argument.StringExpr strValue -> show strValue
  Psl.Argument.IdentifierExpr identifier -> identifier
  Psl.Argument.FuncExpr funcName args -> funcName ++ "(" ++ intercalate ", " (generateArgument <$> args) ++ ")"
  Psl.Argument.ArrayExpr exprs -> "[" ++ intercalate ", " (generateExpression <$> exprs) ++ "]"
  Psl.Argument.NumberExpr numberStr -> numberStr
