module Wasp.Generator.SdkGenerator.UserCore.Common
  ( TemplatesSdkUserCoreProjectDir,
    SdkUserCoreProjectDir,
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
    TemplatesSdkRootDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

-- | Directory of the SDK user core tsconfig project in generated code.
-- It contains all logic dependent on the user's project.
data SdkUserCoreProjectDir

-- | Directory of the SDK user core tsconfig project in templates.
-- It contains all logic dependent on the user's project.
data TemplatesSdkUserCoreProjectDir

mkTmplFd ::
  Path' (Rel TemplatesSdkUserCoreProjectDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkUserCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkUserCoreProjectDir) File' ->
  Path' (Rel TemplatesSdkUserCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFilePath srcFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkUserCoreProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> templatesSdkUserCoreProjectDirInTemplatesSdkRootDir </> srcFilePath)

templatesSdkUserCoreProjectDirInTemplatesSdkRootDir :: Path' (Rel TemplatesSdkRootDir) (Dir TemplatesSdkUserCoreProjectDir)
templatesSdkUserCoreProjectDirInTemplatesSdkRootDir = [reldir|user-core|]

sdkUserCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkUserCoreProjectDir)
sdkUserCoreProjectDirInSdkRootDir = [reldir|user-core|]
