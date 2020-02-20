module Lib
    ( compile
    ) where

import CompileOptions (CompileOptions)
import qualified CompileOptions
import qualified ExternalCode
import qualified Parser
import qualified Generator
import Wasp (setExternalCodeFiles)
import qualified Path
import qualified Path.Aliases as Path


type CompileError = String

compile :: Path.AbsFile -> Path.AbsDir -> CompileOptions -> IO (Either CompileError ())
compile waspFile outDir options = do
    waspStr <- readFile (Path.toFilePath waspFile)

    case Parser.parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> do
            externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
            generateCode $ wasp `setExternalCodeFiles` externalCodeFiles
  where
    generateCode wasp = Generator.writeWebAppCode wasp outDir options >> return (Right ())
