-- | The SDK's MCP modules (experimental): `wasp/server/mcp` (the
-- `context.mcp` and `describe` types, and `dashboardUrl`) and `wasp/client/mcp` (the consent and connections
-- components, and the return-to-consent helper the auth pages use).
module Wasp.Generator.SdkGenerator.McpG
  ( genMcp,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Mcp as AS.Mcp
import qualified Wasp.AppSpec.Expose as AS.Expose
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (genFileCopy, mkTmplFdWithData)
import Wasp.Generator.ServerGenerator.McpG (mcpAppRouteInRootRouter)
import Wasp.Util.Js (makeJsStringLiteral)

genMcp :: AppSpec -> Generator [FileDraft]
genMcp spec = case AS.App.mcp (getApp spec) of
  Nothing -> return []
  Just mcp ->
    sequence
      [ genFileCopy [relfile|server/mcp/index.ts|],
        return $
          mkTmplFdWithData
            [relfile|client/mcp/index.ts|]
            ( object
                [ "mcpAppRouteInRootRouter" .= mcpAppRouteInRootRouter,
                  -- The "require confirmation" toggles only mean something with destructive tools.
                  "hasDestructiveTools" .= if hasDestructiveTools then "true" else "false" :: String
                ]
            ),
        return $
          mkTmplFdWithData
            [relfile|client/mcp/returnTo.ts|]
            ( object
                [ "consentPathLiteral" .= makeJsStringLiteral (routePath (AS.Mcp.consentRoute mcp)),
                  "confirmPathLiteral" .= (makeJsStringLiteral . routePath <$> AS.Mcp.confirmRoute mcp)
                ]
            )
      ]
  where
    routePath ref = AS.Route.path $ AS.resolveRef spec ref
    hasDestructiveTools =
      any (AS.Expose.isDestructive . snd) (AS.getMcpTools spec)
