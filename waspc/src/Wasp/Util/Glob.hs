module Wasp.Util.Glob
  ( Pattern,
    compile,
    match,
    recursiveFileGlobsWithExtensions,
    dirAndDescendantsGlobs,
  )
where

import qualified System.FilePath as FP
import System.FilePath.Glob (Pattern, compile, match)

recursiveFileGlobsWithExtensions :: FilePath -> [String] -> [String]
recursiveFileGlobsWithExtensions dir extensions =
  (\ext -> dir FP.</> "**" FP.</> "*" ++ ext) <$> extensions

dirAndDescendantsGlobs :: FilePath -> [String]
dirAndDescendantsGlobs dir =
  [ dir,
    dir FP.</> "**" FP.</> "*"
  ]
