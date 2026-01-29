module Wasp.Generator.ExternalCodeGenerator.Common
  ( GeneratedExternalCodeDir,
  )
where

-- TODO: (filip) Where should we put this?
-- TODO: consider refactoring the usage of 'GeneratedExternalCodeDir' since
-- generated code might end up in multiple places (e.g. src/ but also public/).
-- Name should probably be narrowed down to something that represents only the
-- src/ directory. Maybe 'GeneratedExtSrcDir'?

-- | Directory where we generate the external code.
-- External code refer to the code files in user's @src/@ directory.
data GeneratedExternalCodeDir
