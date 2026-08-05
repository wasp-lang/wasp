module Wasp.Generator.TypeAugmentationGenerator.Common
  ( TypeAugmentationRootDir,
    TypeAugmentationTemplatesDir,
    typeAugmentationRootDirInGeneratedCodeDir,
    typeAugmentationTemplatesDirInTemplatesDir,
  )
where

import StrongPath
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Templates (TemplatesDir)

data TypeAugmentationRootDir

data TypeAugmentationTemplatesDir

typeAugmentationRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir TypeAugmentationRootDir)
typeAugmentationRootDirInGeneratedCodeDir = [reldir|types|]

typeAugmentationTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir TypeAugmentationTemplatesDir)
typeAugmentationTemplatesDirInTemplatesDir = [reldir|types|]
