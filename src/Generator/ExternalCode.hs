module Generator.ExternalCode
       ( generateExternalCodeDir
       , externalCodeDirPathInSrc
       ) where

import System.FilePath (takeExtension)
import qualified Path
import Path ((</>), reldir)
import qualified Path.Aliases as Path

import CompileOptions (CompileOptions)
import qualified CompileOptions
import Wasp (Wasp)
import qualified Wasp
import qualified Generator.FileDraft as FD
import qualified Generator.Common as Common
import qualified ExternalCode


externalCodeDirPathInSrc :: Path.RelDir
externalCodeDirPathInSrc = [reldir|ext-src|]

generateExternalCodeDir :: CompileOptions -> Wasp -> [FD.FileDraft]
generateExternalCodeDir compileOptions wasp =
    map (generateFile compileOptions) (Wasp.getExternalCodeFiles wasp)

-- | Returns path relative to the root of the generated project.
getExtCodeFileDstPath :: ExternalCode.File -> Path.RelFile
getExtCodeFileDstPath file = Common.srcDirPath </> externalCodeDirPathInSrc </>
                             ExternalCode.getFilePathInExtCodeDir file

getExtCodeFileSrcPath :: CompileOptions -> ExternalCode.File -> Path.AbsFile
getExtCodeFileSrcPath compileOptions file = CompileOptions.externalCodeDirPath compileOptions </>
                                            ExternalCode.getFilePathInExtCodeDir file

generateFile :: CompileOptions -> ExternalCode.File -> FD.FileDraft
generateFile compileOptions file
    | extension `elem` ["js", "jsx"] = generateJsFile file
    | otherwise = FD.createCopyFileDraft (getExtCodeFileDstPath file) (getExtCodeFileSrcPath compileOptions file)
  where
    extension = takeExtension $ Path.toFilePath $ getExtCodeFileSrcPath compileOptions file

-- TODO: Now here we do preprocessing!
generateJsFile :: ExternalCode.File -> FD.FileDraft
generateJsFile file = FD.createTextFileDraft (getExtCodeFileDstPath file) (ExternalCode.getFileText file)



