module Generator.ExternalCode
       ( generateExternalCodeDir
       , externalCodeDirPathInSrc
       ) where

import System.FilePath ((</>), takeExtension)

import CompileOptions (CompileOptions)
import qualified CompileOptions
import Wasp (Wasp)
import qualified Wasp
import qualified Generator.FileDraft as FD
import qualified Generator.Common as Common
import qualified ExternalCode
import qualified Path


externalCodeDirPathInSrc :: FilePath
externalCodeDirPathInSrc = "ext-src"

generateExternalCodeDir :: CompileOptions -> Wasp -> [FD.FileDraft]
generateExternalCodeDir compileOptions wasp =
    map (generateFile compileOptions) (Wasp.getExternalCodeFiles wasp)

getFileDstPath :: ExternalCode.File -> FilePath
getFileDstPath file = Common.srcDirPath </> externalCodeDirPathInSrc </>
                      (Path.toFilePath $ ExternalCode.getFilePathInExtCodeDir file)

getFileSrcPath :: CompileOptions -> ExternalCode.File -> FilePath
getFileSrcPath compileOptions file =
    Path.toFilePath (CompileOptions.externalCodeDirPath compileOptions Path.</>
                     ExternalCode.getFilePathInExtCodeDir file)

generateFile :: CompileOptions -> ExternalCode.File -> FD.FileDraft
generateFile compileOptions file
    | extension `elem` ["js", "jsx"] = generateJsFile file
    | otherwise = FD.createCopyFileDraft (getFileDstPath file) (getFileSrcPath compileOptions file)
  where
    extension = takeExtension (getFileSrcPath compileOptions file)

-- TODO: Now here we do preprocessing!
generateJsFile :: ExternalCode.File -> FD.FileDraft
generateJsFile file = FD.createTextFileDraft (getFileDstPath file) (ExternalCode.getFileText file)



