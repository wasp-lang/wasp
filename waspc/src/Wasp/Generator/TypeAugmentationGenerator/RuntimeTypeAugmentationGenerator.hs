module Wasp.Generator.TypeAugmentationGenerator.RuntimeTypeAugmentationGenerator
  ( genRuntimeTypeAugmentation,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.RuntimeTypeAugmentationGenerator.SdkTypeAugmentationGenerator (genSdkTypeAugmentation)

genRuntimeTypeAugmentation :: AppSpec -> Generator [FileDraft]
genRuntimeTypeAugmentation spec = genSdkTypeAugmentation spec
