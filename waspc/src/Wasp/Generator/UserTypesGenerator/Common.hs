module Wasp.Generator.UserTypesGenerator.Common
  ( UserTypesRootDir,
    UserTypesTemplatesDir,
    mkTmplFdWithDstAndData,
    mkTmplFdWithData,
    mkTmplFd,
    userTypesRootDirInGeneratedCodeDir,
    userTypesTemplatesDirInTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data UserTypesRootDir

data UserTypesTemplatesDir

mkTmplFd :: Path' (Rel UserTypesTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel UserTypesTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel UserTypesTemplatesDir) File' ->
  Path' (Rel UserTypesRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (userTypesRootDirInGeneratedCodeDir </> relDstPath)
    (userTypesTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

userTypesRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir UserTypesRootDir)
userTypesRootDirInGeneratedCodeDir = [reldir|types|]

userTypesTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir UserTypesTemplatesDir)
userTypesTemplatesDirInTemplatesDir = [reldir|types|]
