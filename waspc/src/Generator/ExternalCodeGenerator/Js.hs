module Generator.ExternalCodeGenerator.Js
       ( generateJsFile
       ) where

import StrongPath (Path, Rel, File, (</>))
import qualified Generator.FileDraft as FD
import qualified ExternalCode as EC
import qualified Generator.ExternalCodeGenerator.Common as C


generateJsFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> FD.FileDraft
generateJsFile strategy file = FD.createTextFileDraft dstPath text'
  where
    filePathInSrcExtCodeDir = EC.filePathInExtCodeDir file

    filePathInGenExtCodeDir :: Path (Rel C.GeneratedExternalCodeDir) File
    filePathInGenExtCodeDir = (C.castRelPathFromSrcToGenExtCodeDir filePathInSrcExtCodeDir)

    text = EC.fileText file
    text' = (C._resolveJsFileWaspImports strategy) filePathInGenExtCodeDir text
    dstPath = (C._extCodeDirInProjectRootDir strategy) </> filePathInGenExtCodeDir

