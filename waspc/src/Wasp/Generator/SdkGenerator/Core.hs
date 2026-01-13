module Wasp.Generator.SdkGenerator.Core
  ( genCore,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genCore :: AppSpec -> Generator [FileDraft]
genCore _spec =
  return
    [ mkTmplFd [relfile|tsconfig.json|],
      mkTmplFd [relfile|server/HttpError.ts|]
    ]
