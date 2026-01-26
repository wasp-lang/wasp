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
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkUserCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkUserCoreProjectDir) File' ->
  Path' (Rel TemplatesSdkUserCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkUserCoreProjectDirInSdkRootDir </> destFile)
    (sdkRootDirInTemplatesDir </> templatesSdkUserCoreProjectDirInTemplatesSdkRootDir </> tmplFile)

templatesSdkUserCoreProjectDirInTemplatesSdkRootDir :: Path' (Rel TemplatesSdkRootDir) (Dir TemplatesSdkUserCoreProjectDir)
templatesSdkUserCoreProjectDirInTemplatesSdkRootDir = [reldir|user-core|]

sdkUserCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkUserCoreProjectDir)
sdkUserCoreProjectDirInSdkRootDir = [reldir|user-core|]
