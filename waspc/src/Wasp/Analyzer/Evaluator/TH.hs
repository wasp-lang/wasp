-- | This module exports two TH functions, @makeDeclType@ and @makeEnumType@, which
-- write instances for @IsDeclType@ and @IsEnumType@, respectively. Only correct
-- instances will be generated. If a non-decl or non-enum type name is given to
-- either of these functions, a Haskell type error is raised.
module Wasp.Analyzer.Evaluator.TH
  ( makeDeclType,
    makeEnumType,
  )
where

import Wasp.Analyzer.Evaluator.TH.Decl (makeDeclType)
import Wasp.Analyzer.Evaluator.TH.Enum (makeEnumType)
