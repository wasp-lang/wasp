module Wasp.Generator.SdkGenerator.Core.ApiG
  ( genApi,
  )
where

import StrongPath (relfile)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genApi :: Generator [FileDraft]
genApi =
  return
    [ mkTmplFd [relfile|api/events.ts|],
      mkTmplFd [relfile|api/index.ts|]
    ]
