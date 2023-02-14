module Wasp.Analyzer.TypeDefinitions.TH.Common where

import Language.Haskell.TH
import Wasp.Util (toLowerFirst)

-- | Get an expression representing the string form of a name, starting with a lowercase letter
nameToLowerFirstStringLiteralExpr :: Name -> ExpQ
nameToLowerFirstStringLiteralExpr = litE . stringL . toLowerFirst . nameBase

-- | Get an expression representing the string form of a name
nameToStringLiteralExpr :: Name -> ExpQ
nameToStringLiteralExpr = litE . stringL . nameBase

-- | @genVal name expr@ defines a value binding like @name = expr@
genVal :: Name -> ExpQ -> DecQ
genVal name expr = valD (varP name) (normalB expr) []
