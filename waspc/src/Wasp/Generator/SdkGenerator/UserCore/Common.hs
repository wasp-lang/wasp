module Wasp.Generator.SdkGenerator.UserCore.Common
  ( SdkTemplatesUserCoreProjectDir,
    SdkUserCoreProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
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

-- | Directory of the SDK user core tsconfig project in generated code.
-- It contains all logic dependent on the user's project.
data SdkUserCoreProjectDir

-- | Directory of the SDK user core tsconfig project in templates.
-- It contains all logic dependent on the user's project.
data SdkTemplatesUserCoreProjectDir

mkTmplFd ::
  Path' (Rel SdkTemplatesUserCoreProjectDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    (castRel srcFilePath)
    srcFilePath
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesUserCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    (castRel srcFilePath)
    srcFilePath
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkUserCoreProjectDir) File' ->
  Path' (Rel SdkTemplatesUserCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData destFilePath srcFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkUserCoreProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesUserCoreProjectDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesUserCoreProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesUserCoreProjectDir)
sdkTemplatesUserCoreProjectDirInSdkTemplatesDir = [reldir|user-core|]

sdkUserCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkUserCoreProjectDir)
sdkUserCoreProjectDirInSdkRootDir = [reldir|user-core|]
