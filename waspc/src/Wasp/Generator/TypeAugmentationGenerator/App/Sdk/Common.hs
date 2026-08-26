module Wasp.Generator.TypeAugmentationGenerator.App.Sdk.Common
  ( SdkTypeAugmentationRootDir,
    SdkTypeAugmentationTemplatesDir,
    mkTmplFdWithDstAndData,
    mkTmplFdWithData,
    sdkTypeAugmentationRootDirInGeneratedCodeDir,
    sdkTypeAugmentationTemplatesDirInTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Generator.TypeAugmentationGenerator.App.Common
  ( appTypeAugmentationRootDirInTypeAugmentationRootDir,
    appTypeAugmentationTemplatesDirInTypeAugmentationTemplatesDir,
  )
import Wasp.Generator.TypeAugmentationGenerator.Common
  ( typeAugmentationRootDirInGeneratedCodeDir,
    typeAugmentationTemplatesDirInTemplatesDir,
  )

data SdkTypeAugmentationRootDir

data SdkTypeAugmentationTemplatesDir

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
sdkTypeAugmentationRootDirInGeneratedCodeDir =
  typeAugmentationRootDirInGeneratedCodeDir
    </> appTypeAugmentationRootDirInTypeAugmentationRootDir
    </> [reldir|sdk|]

sdkTypeAugmentationTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTypeAugmentationTemplatesDir)
sdkTypeAugmentationTemplatesDirInTemplatesDir =
  typeAugmentationTemplatesDirInTemplatesDir
    </> appTypeAugmentationTemplatesDirInTypeAugmentationTemplatesDir
    </> [reldir|sdk|]
