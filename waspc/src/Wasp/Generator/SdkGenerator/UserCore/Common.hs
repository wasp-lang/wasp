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
    sdkRootDirInProjectRootDir,
    sdkTemplatesDirInTemplatesDir,
  )

data UserCoreTemplatesDir

userCoreTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir UserCoreTemplatesDir)
userCoreTemplatesDirInSdkTemplatesDir = [reldir|user-core|]

userCoreOutputDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir UserCoreTemplatesDir)
userCoreOutputDirInSdkRootDir = [reldir|user-core|]

mkTmplFd ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData (castRel tmplFile) tmplFile Nothing

mkTmplFdWithData ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData (castRel tmplFile) tmplFile (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel UserCoreTemplatesDir) File' ->
  Path' (Rel UserCoreTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile tmplData =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> userCoreOutputDirInSdkRootDir </> castRel destFile)
    (sdkTemplatesDirInTemplatesDir </> userCoreTemplatesDirInSdkTemplatesDir </> castRel tmplFile)
    tmplData
