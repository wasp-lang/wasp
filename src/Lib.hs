module Lib
    ( compile
    ) where

import qualified Data.Text.IO as TextIO
import Data.Text (Text)
import System.FilePath ((</>))

import qualified Util.IO
import CompileOptions (CompileOptions)
import qualified CompileOptions
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
            externalCodeFiles <- readExternalCodeFiles $ CompileOptions.externalCodeDirPath options
            generateCode $ wasp `setExternalCodeFiles` externalCodeFiles
  where
    generateCode wasp = writeWebAppCode wasp outDir options >> return (Right ())

    -- | Returns paths and contents of external code files.
    -- Paths are relative to the given external code dir path.
    readExternalCodeFiles :: FilePath -> IO [(FilePath, Text)]
    readExternalCodeFiles externalCodeDirPath = do
        externalCodeFilePaths <- Util.IO.listDirectoryDeep externalCodeDirPath
        externalCodeFileContents <- mapM (TextIO.readFile . (externalCodeDirPath </>)) externalCodeFilePaths
        return $ zip externalCodeFilePaths externalCodeFileContents
