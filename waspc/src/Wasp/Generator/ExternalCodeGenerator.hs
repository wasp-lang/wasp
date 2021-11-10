module Wasp.Generator.ExternalCodeGenerator
  ( generateExternalCodeDir,
  )
where

import StrongPath (File', Path', Rel, (</>))
import qualified StrongPath as SP
import qualified System.FilePath as FP
import qualified Wasp.AppSpec.ExternalCode as EC
import qualified Wasp.Generator.ExternalCodeGenerator.Common as C
import Wasp.Generator.ExternalCodeGenerator.Js (generateJsFile)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Wasp (Wasp)
import qualified Wasp.Wasp as Wasp

-- | Takes external code files from Wasp and generates them in new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
generateExternalCodeDir ::
  C.ExternalCodeGeneratorStrategy ->
  Wasp ->
  [FD.FileDraft]
generateExternalCodeDir strategy wasp =
  map (generateFile strategy) (Wasp.getExternalCodeFiles wasp)

generateFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> FD.FileDraft
generateFile strategy file
  | extension `elem` [".js", ".jsx"] = generateJsFile strategy file
  | otherwise =
    let relDstPath = C._extCodeDirInProjectRootDir strategy </> dstPathInGenExtCodeDir
        absSrcPath = EC.fileAbsPath file
     in FD.createCopyFileDraft relDstPath absSrcPath
  where
    dstPathInGenExtCodeDir :: Path' (Rel C.GeneratedExternalCodeDir) File'
    dstPathInGenExtCodeDir = C.castRelPathFromSrcToGenExtCodeDir $ EC.filePathInExtCodeDir file

    extension = FP.takeExtension $ SP.toFilePath $ EC.filePathInExtCodeDir file
