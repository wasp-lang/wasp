module Wasp.Generator.ExternalCodeGenerator.Common
  ( GeneratedExternalCodeDir,
  )
where

-- todo(filip): Where should I put this?
-- TODO: consider refactoring the usage of GeneratedExternalCodeDir since
-- generated code might end up in multiple places (e.g. ext-src/ but also public/).
-- Name should probably be narrowed down to something that represent only the ext-src/
-- directory. Maybe GeneratedExtSrcDir?

-- | Path to the directory where ext code will be generated.
data GeneratedExternalCodeDir
