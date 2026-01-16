module Wasp.Generator.SdkGenerator.Core
  ( genCoreTsconfigProject,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.ApiG (genApi)
import Wasp.Generator.SdkGenerator.Core.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.Core.ClientG (genClient)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)
import Wasp.Generator.SdkGenerator.Core.CoreG (genCoreDir)
import Wasp.Generator.SdkGenerator.Core.DevG (genDev)
import Wasp.Generator.SdkGenerator.Core.EntitiesG (genEntities)
import Wasp.Generator.SdkGenerator.Core.EnvG (genEnv)
import Wasp.Generator.SdkGenerator.Core.ServerG (genServer)
import Wasp.Generator.SdkGenerator.Core.UniversalG (genUniversal)
import Wasp.Util ((<++>))

genCoreTsconfigProject :: AppSpec -> Generator [FileDraft]
genCoreTsconfigProject spec =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|vite-env.d.ts|],
      mkTmplFd [relfile|prisma-runtime-library.d.ts|]
    ]
    <++> genClient spec
    <++> genServer spec
    <++> genAuth spec
    <++> genEntities spec
    <++> genUniversal
    <++> genApi
    <++> genEnv
    <++> genCoreDir spec
    <++> genDev
