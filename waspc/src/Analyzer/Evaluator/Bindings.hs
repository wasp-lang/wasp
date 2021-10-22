module Analyzer.Evaluator.Bindings
  ( Bindings,
    DeclName,
  )
where

import AppSpec.AST.Decl (Decl)
import qualified Data.HashMap.Strict as H

-- | Declarations that have been evaluated so far + names to which they are bound.
type Bindings = H.HashMap DeclName Decl

type DeclName = String
