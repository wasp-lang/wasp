module Wasp.Generator.SdkGenerator.Core.SerilizationG
  ( genCoreDir,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec.Util (hasEntities)
import Wasp.Generator.Common (genOptionally)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import Wasp.Util ((<++>))

genCoreDir :: AppSpec -> Generator [FileDraft]
genCoreDir spec =
  return [mkTmplFd [relfile|core/storage.ts|]]
    <++> genCoreSerialization spec

genCoreSerialization :: AppSpec -> Generator [FileDraft]
genCoreSerialization spec =
  sequence
    [ genCoreSerializationIndex spec,
      return $ mkTmplFd [relfile|core/serialization/custom-register.ts|]
    ]
    <++> genOptionally (hasEntities spec) (mkTmplFd [relfile|core/serialization/prisma.ts|])

genCoreSerializationIndex :: AppSpec -> Generator FileDraft
genCoreSerializationIndex spec =
  return $ mkTmplFdWithData [relfile|core/serialization/index.ts|] tmplData
  where
    tmplData = object ["entitiesExist" .= hasEntities spec]
