module Wasp.Generator.SdkGenerator.Core.Common
  ( SdkCoreProjectDir,
    SdkTemplatesCoreProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDestAndData,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, castRel, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

-- | Directory of the SDK core tsconfig project in generated code.
-- It contains all logic not dependent on the user's project.
data SdkCoreProjectDir

-- | Directory of the SDK core tsconfig project in templates.
-- It contains all logic not dependent on the user's project.
data SdkTemplatesCoreProjectDir

mkTmplFd ::
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkCoreProjectDir) File' ->
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFilePath srcFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkCoreProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesCoreProjectDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesCoreProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesCoreProjectDir)
sdkTemplatesCoreProjectDirInSdkTemplatesDir = [reldir|core|]

sdkCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkCoreProjectDir)
sdkCoreProjectDirInSdkRootDir = [reldir|core|]
