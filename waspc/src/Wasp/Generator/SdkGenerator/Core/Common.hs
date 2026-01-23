module Wasp.Generator.SdkGenerator.Core.Common
  ( CoreTemplatesDir,
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
    sdkTemplatesDirInTemplatesDir,
  )

-- | Directory of the SDK core tsconfig project.
-- It contains all logic not dependent on the user's project.
data CoreTemplatesDir

mkTmplFd ::
  Path' (Rel CoreTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData tmplFile tmplFile Nothing

mkTmplFdWithData ::
  Path' (Rel CoreTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData tmplFile tmplFile (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel CoreTemplatesDir) File' ->
  Path' (Rel CoreTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> coreTemplatesDirInSdkRootDir </> destFile)
    (sdkTemplatesDirInTemplatesDir </> coreTemplatesDirInSdkTemplatesDir </> tmplFile)

coreTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir CoreTemplatesDir)
coreTemplatesDirInSdkTemplatesDir = [reldir|core|]

coreTemplatesDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir CoreTemplatesDir)
coreTemplatesDirInSdkRootDir = castRel coreTemplatesDirInSdkTemplatesDir
