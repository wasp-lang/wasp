module Lib (
  compile
) where

type CompileError = String

compile :: FilePath -> FilePath -> IO (Either CompileError ())
compile waspFile outDir = undefined
