module Generator.ExternalCode
       ( generateExternalCodeDir
       ) where

import qualified System.FilePath as FP
import qualified Path

import CompileOptions (CompileOptions)
import Wasp (Wasp)
import qualified Wasp
import qualified ExternalCode
import qualified Generator.FileDraft as FD
import qualified Generator.ExternalCode.Common as Common
import Generator.ExternalCode.Js (generateJsFile)


generateExternalCodeDir :: CompileOptions -> Wasp -> [FD.FileDraft]
generateExternalCodeDir compileOptions wasp =
    map (generateFile compileOptions) (Wasp.getExternalCodeFiles wasp)

generateFile :: CompileOptions -> ExternalCode.File -> FD.FileDraft
generateFile compileOptions file
    | extension `elem` [".js", ".jsx"] = generateJsFile file
    | otherwise = FD.createCopyFileDraft (Common.getExtCodeFileDstPath file)
                                         (Common.getExtCodeFileSrcPath compileOptions file)
  where
    extension = FP.takeExtension $ Path.toFilePath $ Common.getExtCodeFileSrcPath compileOptions file



