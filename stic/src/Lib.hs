module Lib
    ( compile
    ) where

import Generator
import Wasp

type CompileError = String

compile :: FilePath -> FilePath -> IO (Either CompileError ())
compile waspFile outDir = do
    writeWebAppCode (fromApp $ App "test" "Test!") "testOutput"
    return $ Right ()
