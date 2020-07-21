module Generator.ExternalCodeGenerator.Js
       ( generateJsFile
       ) where

import qualified Path
import qualified Generator.FileDraft as FD
import qualified ExternalCode as EC
import qualified Generator.ExternalCodeGenerator.Common as C

generateJsFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> FD.FileDraft
generateJsFile strategy file = FD.createTextFileDraft dstPath text'
  where
    text = EC.fileText file
    text' = (C._resolveJsFileWaspImports strategy) (EC.filePathInExtCodeDir file) text
    dstPath = (C._extCodeDirInProjectRootDir strategy) Path.</> EC.filePathInExtCodeDir file

