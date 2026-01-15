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
  sequence
    [ return $ mkTmplFd [relfile|client/hooks.ts|],
      -- Not migrated to TS yet
      return $ mkTmplFd [relfile|client/operations/internal/updateHandlersMap.js|],
      return $ mkTmplFd [relfile|client/operations/queryClient.ts|],
      return $ mkTmplFd [relfile|client/router/types.ts|],
      return $ mkTmplFd [relfile|client/router/linkHelpers.ts|]
    ]
