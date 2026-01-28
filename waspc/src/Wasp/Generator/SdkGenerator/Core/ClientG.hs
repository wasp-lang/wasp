module Wasp.Generator.SdkGenerator.Core.ClientG
  ( genClient,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Client.RouterG (genClientRouter)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)
import Wasp.Util ((<++>))

genClient :: AppSpec -> Generator [FileDraft]
genClient spec =
  return
    [ mkTmplFd [relfile|client/index.ts|],
      mkTmplFd [relfile|client/hooks.ts|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/resources.js|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/updateHandlersMap.js|],
      mkTmplFd [relfile|client/operations/rpc.ts|],
      mkTmplFd [relfile|client/operations/queryClient.ts|]
    ]
    <++> genClientRouter spec
