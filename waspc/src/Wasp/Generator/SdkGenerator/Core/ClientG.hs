module Wasp.Generator.SdkGenerator.Core.ClientG
  ( genClient,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import StrongPath (reldir, relfile, (</>))
import StrongPath.Types
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Client.RouterG (genClientRouter)
import Wasp.Generator.SdkGenerator.Core.Common (CoreTemplatesDir, mkTmplFd, mkTmplFdWithData)
import Wasp.Util ((<++>))
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genClient :: AppSpec -> Generator [FileDraft]
genClient spec =
  return
    [ mkTmplFd [relfile|client/index.ts|],
      mkTmplFd [relfile|client/hooks.ts|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/resources.js|],
      -- Not migrated to TS yet
      mkTmplFd [relfile|client/operations/internal/updateHandlersMap.js|],
      mkTmplFd [relfile|client/operations/queryClient.ts|]
    ]
    <++> genClientRouter spec
