module Wasp.Generator.TypeAugmentationGenerator
  ( genTypeAugmentation,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.AppTypeAugmentationGenerator (genAppTypeAugmentation)
import Wasp.Generator.TypeAugmentationGenerator.SpecTypeAugmentationGenerator (genSpecTypeAugmentation)
import Wasp.Util ((<++>))

genTypeAugmentation :: AppSpec -> Generator [FileDraft]
genTypeAugmentation spec =
  genAppTypeAugmentation spec
    <++> genSpecTypeAugmentation spec
