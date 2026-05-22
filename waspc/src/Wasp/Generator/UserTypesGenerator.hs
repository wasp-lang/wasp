module Wasp.Generator.UserTypesGenerator
  ( genUserTypes,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec, getEntities)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.UserTypesGenerator.Common (mkTmplFdWithData)

genUserTypes :: AppSpec -> Generator [FileDraft]
genUserTypes spec = genSpecModuleAugmentation spec

genSpecModuleAugmentation :: AppSpec -> Generator [FileDraft]
genSpecModuleAugmentation spec =
  return
    [ mkTmplFdWithData
        [relfile|register-spec.ts|]
        (object ["entities" .= entities])
    ]
  where
    entities = map (makeJsonWithEntityData . fst) $ getEntities spec
