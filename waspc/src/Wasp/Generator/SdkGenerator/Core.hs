module Wasp.Generator.SdkGenerator.Core
  ( genCoreTsconfigProject,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.ApiG (genApi)
import Wasp.Generator.SdkGenerator.Core.ClientG (genClient)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)
import Wasp.Generator.SdkGenerator.Core.CoreG (genCoreDir)
import Wasp.Generator.SdkGenerator.Core.EntitiesG (genEntities)
import Wasp.Generator.SdkGenerator.Core.EnvG (genEnv)
import Wasp.Generator.SdkGenerator.Core.UniversalG (genUniversal)
import Wasp.Util ((<++>))

genCoreTsconfigProject :: AppSpec -> Generator [FileDraft]
genCoreTsconfigProject spec =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|server/HttpError.ts|],
      mkTmplFd [relfile|server/types/index.ts|],
      mkTmplFd [relfile|server/jobs/core/job.ts|],
      mkTmplFd [relfile|server/middleware/globalMiddleware.ts|],
      mkTmplFd [relfile|vite-env.d.ts|],
      mkTmplFd [relfile|prisma-runtime-library.d.ts|],
      mkTmplFd [relfile|auth/password.ts|],
      mkTmplFd [relfile|auth/forms/internal/util.ts|]
    ]
    <++> genClient spec
    <++> genEntities spec
    <++> genUniversal
    <++> genApi
    <++> genEnv
    <++> genCoreDir spec
