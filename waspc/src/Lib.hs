module Lib
    ( compile
    , ProjectRootDir
    ) where

import StrongPath (Path, Abs, File, Dir)
import qualified StrongPath as SP
import CompileOptions (CompileOptions)
import qualified CompileOptions
import qualified ExternalCode
import qualified Parser
import qualified Generator
import Wasp (setExternalCodeFiles)
import Generator.Common (ProjectRootDir)


type CompileError = String

compile :: Path Abs File -> Path Abs (Dir ProjectRootDir)-> CompileOptions -> IO (Either CompileError ())
compile waspFile outDir options = do
    waspStr <- readFile (SP.toFilePath waspFile)

    case Parser.parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> do
            externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
            generateCode $ wasp `setExternalCodeFiles` externalCodeFiles
  where
    generateCode wasp = Generator.writeWebAppCode wasp outDir options >> return (Right ())
