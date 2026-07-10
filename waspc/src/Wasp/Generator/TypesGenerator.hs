module Wasp.Generator.TypesGenerator
  ( genTypes,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypesGenerator.SdkTypesGenerator (genSdkTypes)
import Wasp.Generator.TypesGenerator.SpecTypesGenerator (genSpecTypes)
import Wasp.Util ((<++>))

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  genSpecTypes spec
    <++> genSdkTypes spec
