module Wasp.Generator.TypeAugmentationGenerator.App
  ( genAppTypeAugmentation,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.App.Sdk (genSdkTypeAugmentation)

genAppTypeAugmentation :: AppSpec -> Generator [FileDraft]
genAppTypeAugmentation spec = genSdkTypeAugmentation spec
