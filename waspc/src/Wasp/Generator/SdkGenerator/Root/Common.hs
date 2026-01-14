module Wasp.Generator.SdkGenerator.Root.Common
  ( RootTemplatesDir,
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
    sdkRootDirInProjectRootDir,
    sdkTemplatesDirInTemplatesDir,
  )

-- | Directory of the SDK root tsconfig project.
-- References all other SDK tsconfig projects.
data RootTemplatesDir

mkTmplFd ::
  Path' (Rel RootTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData tmplFile tmplFile Nothing

mkTmplFdWithData ::
  Path' (Rel RootTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData tmplFile tmplFile (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel RootTemplatesDir) File' ->
  Path' (Rel RootTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> rootTemplatesDirInSdkRootDir </> destFile)
    (sdkTemplatesDirInTemplatesDir </> rootTemplatesDirInSdkTemplatesDir </> tmplFile)

rootTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir RootTemplatesDir)
rootTemplatesDirInSdkTemplatesDir = [reldir|.|]

rootTemplatesDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir RootTemplatesDir)
rootTemplatesDirInSdkRootDir = castRel rootTemplatesDirInSdkTemplatesDir
