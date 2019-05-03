module Lib
    ( compile
    ) where

import Parser
import Generator


type CompileError = String

compile :: FilePath -> FilePath -> IO (Either CompileError ())
compile waspFile outDir = do
    waspStr <- readFile waspFile

    case parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> generateCode wasp
  where
    generateCode wasp = writeWebAppCode wasp outDir >> return (Right ())
