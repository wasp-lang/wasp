module Wasp.Generator.TypeAugmentationGenerator.AppTypeAugmentationGenerator
  ( genAppTypeAugmentation,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.AppTypeAugmentationGenerator.SdkTypeAugmentationGenerator (genSdkTypeAugmentation)

genAppTypeAugmentation :: AppSpec -> Generator [FileDraft]
genAppTypeAugmentation spec = genSdkTypeAugmentation spec
