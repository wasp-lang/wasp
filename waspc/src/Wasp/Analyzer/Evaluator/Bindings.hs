module Wasp.Analyzer.Evaluator.Bindings
  ( Bindings,
    DeclName,
  )
where

import qualified Data.HashMap.Strict as H
import Wasp.AppSpec.Core.Decl (Decl)

-- | Declarations that have been evaluated so far + names to which they are bound.
type Bindings = H.HashMap DeclName Decl

type DeclName = String
