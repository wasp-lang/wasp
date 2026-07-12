module Wasp.Generator.TypeAugmentationGenerator.RuntimeTypeAugmentationGenerator.SdkTypeAugmentationGenerator.Common
  ( SdkTypeAugmentationRootDir,
    SdkTypeAugmentationTemplatesDir,
    mkTmplFdWithDstAndData,
    mkTmplFdWithData,
    mkTmplFd,
    sdkTypeAugmentationRootDirInGeneratedCodeDir,
    sdkTypeAugmentationTemplatesDirInTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data SdkTypeAugmentationRootDir

data SdkTypeAugmentationTemplatesDir

mkTmplFd :: Path' (Rel SdkTypeAugmentationTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTypeAugmentationTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTypeAugmentationTemplatesDir) File' ->
  Path' (Rel SdkTypeAugmentationRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkTypeAugmentationRootDirInGeneratedCodeDir </> relDstPath)
    (sdkTypeAugmentationTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

sdkTypeAugmentationRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir SdkTypeAugmentationRootDir)
sdkTypeAugmentationRootDirInGeneratedCodeDir = [reldir|types/runtime/sdk|]

sdkTypeAugmentationTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTypeAugmentationTemplatesDir)
sdkTypeAugmentationTemplatesDirInTemplatesDir = [reldir|types/runtime/sdk|]
