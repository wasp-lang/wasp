module Wasp.Generator.ExternalCodeGenerator.Common
  ( GeneratedExternalCodeDir,
  )
where

-- TODO: (filip) Where should we put this?
-- TODO: consider refactoring the usage of 'GeneratedExternalCodeDir' since
-- generated code might end up in multiple places (e.g. src/ but also public/).
-- Name should probably be narrowed down to something that represents only the
-- src/ directory. Maybe 'GeneratedExtSrcDir'?

-- | The term 'External code' refers to the code users write in their src directory
-- This type represents the folder containing user-written code inside our generated SDK code.
data GeneratedExternalCodeDir
