module Wasp.Analyzer.TypeDefinitions.TH.Common where

import Language.Haskell.TH
import Wasp.Util (toLowerFirst)

-- | Get an expression representing the string form of a name, starting with a lowercase letter
nameToLowerFirstStringLiteralExpr :: Name -> ExpQ
nameToLowerFirstStringLiteralExpr = litE . stringL . toLowerFirst . nameBase

-- | Get an expression representing the string form of a name
nameToStringLiteralExpr :: Name -> ExpQ
nameToStringLiteralExpr = litE . stringL . nameBase

-- | @genFunc name expr@ writes a function like @name = expr@
-- TODO: This function should use `valD` instead of `funD` because we're declaring a value
genFunc :: Name -> ExpQ -> DecQ
genFunc name expr = funD name [clause [] (normalB expr) []]
