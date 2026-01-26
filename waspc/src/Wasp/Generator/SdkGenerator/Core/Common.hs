module Wasp.Generator.SdkGenerator.Core.Common
  ( SdkCoreProjectDir,
    TemplatesSdkCoreProjectDir,
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

-- | Directory of the SDK core tsconfig project in generated code.
-- It contains all logic not dependent on the user's project.
data SdkCoreProjectDir

-- | Directory of the SDK core tsconfig project in templates.
-- It contains all logic not dependent on the user's project.
data TemplatesSdkCoreProjectDir

mkTmplFd ::
  Path' (Rel TemplatesSdkCoreProjectDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkCoreProjectDir) File' ->
  Path' (Rel TemplatesSdkCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkCoreProjectDirInSdkRootDir </> destFile)
    (sdkRootDirInTemplatesDir </> templatesSdkCoreProjectDirInTemplatesSdkRootDin </> tmplFile)

templatesSdkCoreProjectDirInTemplatesSdkRootDin :: Path' (Rel TemplatesSdkRootDir) (Dir TemplatesSdkCoreProjectDir)
templatesSdkCoreProjectDirInTemplatesSdkRootDin = [reldir|core|]

sdkCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkCoreProjectDir)
sdkCoreProjectDirInSdkRootDir = [reldir|core|]
