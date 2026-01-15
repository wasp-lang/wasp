module Wasp.Generator.SdkGenerator.Core
  ( genCoreTsconfigProject,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.ApiG (genApi)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)
import Wasp.Generator.SdkGenerator.Core.CoreG (genCoreDir)
import Wasp.Generator.SdkGenerator.Core.UniversalG (genUniversal)
import Wasp.Util ((<++>))

genCoreTsconfigProject :: AppSpec -> Generator [FileDraft]
genCoreTsconfigProject spec =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|server/HttpError.ts|]
    ]
    <++> genUniversal
    <++> genApi
    <++> genCoreDir spec
