module Wasp.Generator.SdkGenerator.Core.Common
  ( SdkCoreDir,
    SdkTemplatesCoreDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
    genFileCopy,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, castRel, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

-- | Directory of the SDK core tsconfig project in generated code.
-- It contains all logic not dependent on the user's project.
data SdkCoreDir

-- | Directory of the SDK core tsconfig project in templates.
-- It contains all logic not dependent on the user's project.
data SdkTemplatesCoreDir

genFileCopy :: Path' (Rel SdkTemplatesCoreDir) File' -> Generator FileDraft
genFileCopy = return . mkTmplFd

mkTmplFd ::
  Path' (Rel SdkTemplatesCoreDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesCoreDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesCoreDir) File' ->
  Path' (Rel SdkCoreDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcFilePath destFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkCoreDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesCoreDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesCoreDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesCoreDir)
sdkTemplatesCoreDirInSdkTemplatesDir = [reldir|core|]

sdkCoreDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkCoreDir)
sdkCoreDirInSdkRootDir = [reldir|core|]
