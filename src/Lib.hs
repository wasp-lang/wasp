module Lib
    ( compile
    ) where

import CompileOptions (CompileOptions)
import Parser
import Generator


type CompileError = String

compile :: FilePath -> FilePath -> CompileOptions -> IO (Either CompileError ())
compile waspFile outDir options = do
    waspStr <- readFile waspFile

    case parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> generateCode wasp
  where
    generateCode wasp = writeWebAppCode wasp outDir options >> return (Right ())
