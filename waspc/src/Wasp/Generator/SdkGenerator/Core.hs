module Wasp.Generator.SdkGenerator.Core
  ( genCoreTsconfigProject,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genCoreTsconfigProject :: AppSpec -> Generator [FileDraft]
genCoreTsconfigProject _spec =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|server/HttpError.ts|]
    ]
