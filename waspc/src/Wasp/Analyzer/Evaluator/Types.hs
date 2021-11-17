module Wasp.Analyzer.Evaluator.Types
  ( ExtImport (..),
    JSON (..),
    PSL (..),
  )
where

-- TODO:
-- After refactoring "Analyzer.StdTypeDefinitions" to use types in "Wasp"
-- module, this module can be deleted and the correct types from there will
-- be used in the result of evaluation.

import Wasp.Analyzer.TypeChecker.AST (ExtImportName)
-- TODO: For now I imported PSL from AppSpec here and I just reexport it, to avoid too much refactoring.
--   I should figure out if there is a better way to do it -> maybe I should directly import from AppSpec every time.
--   Or maybe this is the best way, to have this "proxy" module. Not sure yet. I would have to figure out what is
--   PSL actually, what is JSON, what kind of types are those, how do we call them / treat them.
--   One way to look at it is: they are "hardcoded types", in the sense that we have no "abstract" support for them
--   in the language. They are not primitive types, they need special handling, from parser to evaluator,
--   and they are also not declarations or enums so that we can abstract them.
--   Only way I see to get rid of "hardcoding" is to somehow abstract over them -> e.g. by introducing quoters as a thing.
--   Or maybe introducing something like "abstract type" where we provide parser, type info and evaluator all as a single
--   bundle that explains this type and can be dependency injected.
import Wasp.AppSpec.Entity (PSL (..))

data ExtImport = ExtImport ExtImportName String deriving (Eq, Show)

newtype JSON = JSON String deriving (Eq, Show)
