module Wasp.Generator.SdkGenerator.Core.ClientG
  ( genClient,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)

genClient :: AppSpec -> Generator [FileDraft]
genClient _spec =
  return
    [ mkTmplFd [relfile|client/hooks.ts|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/resources.js|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/updateHandlersMap.js|],
      mkTmplFd [relfile|client/operations/queryClient.ts|],
      mkTmplFd [relfile|client/router/types.ts|],
      mkTmplFd [relfile|client/router/linkHelpers.ts|]
    ]
