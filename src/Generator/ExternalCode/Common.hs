module Generator.ExternalCode.Common
       ( externalCodeDirPathInSrc
       , getExtCodeFileDstPath
       , getExtCodeFileSrcPath
       ) where

import Path ((</>), reldir)
import qualified Path.Aliases as Path

import CompileOptions (CompileOptions)
import qualified CompileOptions
import qualified ExternalCode
import qualified Generator.Common as Common


externalCodeDirPathInSrc :: Path.RelDir
externalCodeDirPathInSrc = [reldir|ext-src|]

-- | Returns path where external code file will be generated,
-- relative to the root of the generated project.
getExtCodeFileDstPath :: ExternalCode.File -> Path.RelFile
getExtCodeFileDstPath file = Common.srcDirPath </> externalCodeDirPathInSrc </>
                             ExternalCode.getFilePathInExtCodeDir file

-- | Returns absolute path of the original external code file.
getExtCodeFileSrcPath :: CompileOptions -> ExternalCode.File -> Path.AbsFile
getExtCodeFileSrcPath compileOptions file = CompileOptions.externalCodeDirPath compileOptions </>
                                            ExternalCode.getFilePathInExtCodeDir file



