module Wasp.Generator.SdkGenerator.Core
  ( genCoreTsconfigProject,
  )
where

import StrongPath (relfile)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genCoreTsconfigProject :: Generator [FileDraft]
genCoreTsconfigProject =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|server/HttpError.ts|]
    ]
