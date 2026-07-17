module Wasp.Util.Glob
  ( GlobPatterns,
    compileGlobPatterns,
    matchesAnyGlob,
  )
where

import qualified System.FilePath.Glob as Glob

type GlobPatterns = [Glob.Pattern]

compileGlobPatterns :: [String] -> GlobPatterns
compileGlobPatterns = map Glob.compile

matchesAnyGlob :: GlobPatterns -> FilePath -> Bool
matchesAnyGlob patterns path = any (`Glob.match` path) patterns
