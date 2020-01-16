module Lib
    ( compile
    ) where

import CompileOptions (CompileOptions)
import qualified CompileOptions
import qualified ExternalCode
import Parser
import Generator
import Wasp (setExternalCodeFiles)


type CompileError = String

compile :: FilePath -> FilePath -> CompileOptions -> IO (Either CompileError ())
compile waspFile outDir options = do
    waspStr <- readFile waspFile

    case parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> do
            externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
            generateCode $ wasp `setExternalCodeFiles` externalCodeFiles
  where
    generateCode wasp = writeWebAppCode wasp outDir options >> return (Right ())
