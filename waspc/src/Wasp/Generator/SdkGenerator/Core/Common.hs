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
    SdkTemplatesDir,
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
mkTmplFd srcFilePath =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkCoreProjectDir) File' ->
  Path' (Rel TemplatesSdkCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFilePath srcFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkCoreProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> templatesSdkCoreProjectDirInSdkTemplatesDir </> srcFilePath)

templatesSdkCoreProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir TemplatesSdkCoreProjectDir)
templatesSdkCoreProjectDirInSdkTemplatesDir = [reldir|core|]

sdkCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkCoreProjectDir)
sdkCoreProjectDirInSdkRootDir = [reldir|core|]
