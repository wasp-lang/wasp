module Wasp.Generator.SdkGenerator.UserCore.Common
  ( UserCoreTemplatesDir,
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

-- | Directory of the SDK user core tsconfig project.
-- It contains all logic dependent on the user's project.
data UserCoreTemplatesDir

mkTmplFd ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData tmplFile tmplFile Nothing

mkTmplFdWithData ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData tmplFile tmplFile (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  Path' (Rel UserCoreTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> userCoreOutputDirInSdkRootDir </> destFile)
    (sdkTemplatesDirInTemplatesDir </> userCoreTemplatesDirInSdkTemplatesDir </> tmplFile)

userCoreTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir UserCoreTemplatesDir)
userCoreTemplatesDirInSdkTemplatesDir = [reldir|user-core|]

userCoreOutputDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir UserCoreTemplatesDir)
userCoreOutputDirInSdkRootDir = castRel userCoreTemplatesDirInSdkTemplatesDir
