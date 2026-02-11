module Wasp.Generator.SdkGenerator.Core.ClientG
  ( genClient,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Client.RouterG (genClientRouter)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Util ((<++>))

genClient :: AppSpec -> Generator [FileDraft]
genClient spec =
  return
    [ mkTmplFd [relfile|client/index.ts|],
      mkTmplFd [relfile|client/hooks.ts|],
      mkTmplFd [relfile|client/waspEnv.ts|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/resources.js|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/updateHandlersMap.js|],
      mkTmplFd [relfile|client/operations/rpc.ts|],
      mkTmplFd [relfile|client/operations/queryClient.ts|],
      mkTmplFd [relfile|client/operations/internal/index.ts|],
      mkTmplFd [relfile|client/operations/queries/core.ts|],
      mkTmplFd [relfile|client/operations/actions/core.ts|],
      mkTmplFd [relfile|client/operations/hooks.ts|],
      mkTmplFd [relfile|client/test/vitest/helpers.tsx|],
      mkTmplFd [relfile|client/test/index.ts|],
      mkTmplFd [relfile|client/crud/operationsHelpers.ts|]
    ]
    <++> genClientRouter spec
    <++> genClientEnvWaspSchema
    <++> genClientConfigFile

genClientConfigFile :: Generator [FileDraft]
genClientConfigFile =
  return [mkTmplFdWithData [relfile|client/config.ts|] tmplData]
  where
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName
        ]

genClientEnvWaspSchema :: Generator [FileDraft]
genClientEnvWaspSchema = return [mkTmplFdWithData tmplPath tmplData]
  where
    tmplPath = [relfile|client/env/waspSchema.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName,
          "defaultServerUrl" .= Server.defaultDevServerUrl
        ]
