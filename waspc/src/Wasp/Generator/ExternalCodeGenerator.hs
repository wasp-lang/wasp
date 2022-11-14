module Wasp.Generator.ExternalCodeGenerator
  ( genExternalCodeDir,
  )
where

import Data.Maybe (mapMaybe)
import StrongPath (File', Path', Rel, (</>))
import qualified StrongPath as SP
import qualified System.FilePath as FP
import qualified Wasp.AppSpec.ExternalCode as EC
import qualified Wasp.Generator.ExternalCodeGenerator.Common as C
import Wasp.Generator.ExternalCodeGenerator.Js (genSourceFile)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.Monad (Generator)

-- | Takes external code files from Wasp and generates them in new location as part of the generated project.
-- It might not just copy them but also do some changes on them, as needed.
genExternalCodeDir ::
  C.ExternalCodeGeneratorStrategy ->
  [EC.File] ->
  Generator [FD.FileDraft]
genExternalCodeDir strategy = sequence . mapMaybe (genFile strategy)

genFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> Maybe (Generator FD.FileDraft)
genFile strategy file
  | fileName == "tsconfig.json" = Nothing
  | extension `elem` [".js", ".jsx", ".ts", ".tsx"] = Just $ genSourceFile strategy file
  | otherwise = Just $ genResourceFile strategy file
  where
    extension = FP.takeExtension filePath
    fileName = FP.takeFileName filePath
    filePath = SP.toFilePath $ EC.filePathInExtCodeDir file

genResourceFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> Generator FileDraft
genResourceFile strategy file = return $ FD.createCopyFileDraft relDstPath absSrcPath
  where
    relDstPath = C._extCodeDirInProjectRootDir strategy </> dstPathInGenExtCodeDir
    absSrcPath = EC.fileAbsPath file
    dstPathInGenExtCodeDir :: Path' (Rel C.GeneratedExternalCodeDir) File'
    dstPathInGenExtCodeDir = C.castRelPathFromSrcToGenExtCodeDir $ EC.filePathInExtCodeDir file