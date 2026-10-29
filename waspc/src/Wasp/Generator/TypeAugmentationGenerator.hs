module Wasp.Generator.TypeAugmentationGenerator
  ( genTypeAugmentation,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypeAugmentationGenerator.App (genAppTypeAugmentation)
import Wasp.Generator.TypeAugmentationGenerator.Spec (genSpecTypeAugmentation)
import Wasp.Util ((<++>))

genTypeAugmentation :: AppSpec -> Generator [FileDraft]
genTypeAugmentation spec =
  genAppTypeAugmentation spec
    <++> genSpecTypeAugmentation spec
