module Wasp.Generator.TypeAugmentationGenerator.AppTypeAugmentationGenerator.Common
  ( AppTypeAugmentationRootDir,
    AppTypeAugmentationTemplatesDir,
    appTypeAugmentationRootDirInTypeAugmentationRootDir,
    appTypeAugmentationTemplatesDirInTypeAugmentationTemplatesDir,
  )
where

import StrongPath
import Wasp.Generator.TypeAugmentationGenerator.Common (TypeAugmentationRootDir, TypeAugmentationTemplatesDir)

data AppTypeAugmentationRootDir

data AppTypeAugmentationTemplatesDir

appTypeAugmentationRootDirInTypeAugmentationRootDir :: Path' (Rel TypeAugmentationRootDir) (Dir AppTypeAugmentationRootDir)
appTypeAugmentationRootDirInTypeAugmentationRootDir = [reldir|app|]

appTypeAugmentationTemplatesDirInTypeAugmentationTemplatesDir :: Path' (Rel TypeAugmentationTemplatesDir) (Dir AppTypeAugmentationTemplatesDir)
appTypeAugmentationTemplatesDirInTypeAugmentationTemplatesDir = [reldir|app|]
