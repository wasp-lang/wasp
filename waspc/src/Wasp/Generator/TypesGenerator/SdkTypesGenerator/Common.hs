module Wasp.Generator.TypesGenerator.SdkTypesGenerator.Common
  ( SdkTypesRootDir,
    SdkTypesTemplatesDir,
    mkTmplFdWithDstAndData,
    mkTmplFdWithData,
    mkTmplFd,
    sdkTypesRootDirInGeneratedCodeDir,
    sdkTypesTemplatesDirInTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data SdkTypesRootDir

data SdkTypesTemplatesDir

mkTmplFd :: Path' (Rel SdkTypesTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTypesTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTypesTemplatesDir) File' ->
  Path' (Rel SdkTypesRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkTypesRootDirInGeneratedCodeDir </> relDstPath)
    (sdkTypesTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

sdkTypesRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir SdkTypesRootDir)
sdkTypesRootDirInGeneratedCodeDir = [reldir|types/sdk|]

sdkTypesTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTypesTemplatesDir)
sdkTypesTemplatesDirInTemplatesDir = [reldir|types/sdk|]
