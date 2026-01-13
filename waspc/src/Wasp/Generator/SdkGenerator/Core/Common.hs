module Wasp.Generator.SdkGenerator.Core.Common
  ( CoreTemplatesDir,
    mkTmplFd,
  )
where

import StrongPath (Dir, File', Path', Rel, castRel, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInProjectRootDir,
    sdkTemplatesDirInTemplatesDir,
  )

data CoreTemplatesDir

coreTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir CoreTemplatesDir)
coreTemplatesDirInSdkTemplatesDir = [reldir|core|]

coreOutputDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir CoreTemplatesDir)
coreOutputDirInSdkRootDir = [reldir|core|]

mkTmplFd ::
  Path' (Rel CoreTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> coreOutputDirInSdkRootDir </> castRel tmplFile)
    (sdkTemplatesDirInTemplatesDir </> coreTemplatesDirInSdkTemplatesDir </> castRel tmplFile)
    Nothing
