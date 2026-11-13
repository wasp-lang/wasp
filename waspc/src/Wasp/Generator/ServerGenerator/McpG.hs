-- | The app's MCP server (experimental): the tool registry built from the
-- operations with `expose.mcp`, and the routes that mount it.
module Wasp.Generator.ServerGenerator.McpG
  ( genMcp,
    mcpAppRouteInRootRouter,
    mcpSecretEnvVarName,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import StrongPath (Dir, Path, Posix, Rel, reldirP, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Mcp as AS.Mcp
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.Expose as AS.Expose
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToAliasedImportJson)
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Util.Js (makeJsStringLiteral)

-- | Where the consent and connections components' endpoints live.
mcpAppRouteInRootRouter :: String
mcpAppRouteInRootRouter = "mcp-app"

-- | Seals OAuth codes and tokens and signs `requestState`.
mcpSecretEnvVarName :: String
mcpSecretEnvVarName = "MCP_SECRET"

genMcp :: AppSpec -> Generator [FileDraft]
genMcp spec = case (AS.App.mcp app, AS.App.auth app) of
  (Just mcp, Just auth) -> return [genMcpIndex spec mcp auth, genMcpRouter]
  _ -> return []
  where
    app = getApp spec

genMcpRouter :: FileDraft
genMcpRouter =
  C.mkTmplFdWithDstAndData
    (C.asTmplFile [relfile|src/mcp/router.ts|])
    (C.asServerFile [relfile|src/mcp/router.ts|])
    Nothing

genMcpIndex :: AppSpec -> AS.Mcp.Mcp -> AS.Auth.Auth -> FileDraft
genMcpIndex spec mcp auth =
  C.mkTmplFdWithDstAndData
    (C.asTmplFile [relfile|src/mcp/index.ts|])
    (C.asServerFile [relfile|src/mcp/index.ts|])
    (Just tmplData)
  where
    app = getApp spec
    tmplData =
      object
        [ "tools" .= map toolData (AS.getMcpTools spec),
          "appNameLiteral" .= makeJsStringLiteral (AS.App.name app),
          "appTitleLiteral" .= makeJsStringLiteral (AS.App.title app),
          "instructionsLiteral" .= makeJsStringLiteral (AS.Mcp.instructions mcp),
          "whoami" .= jsBool (AS.Mcp.isWhoamiEnabled mcp),
          "consentPathLiteral" .= makeJsStringLiteral (AS.Route.path consentRoute),
          "userEntity" .= makeJsonWithEntityData (AS.Entity.getName userEntity),
          "userIdFromString" .= userIdFromString "userId",
          "scopesLiteral" .= scopesLiteral (AS.Mcp.getScopes mcp),
          "confirmPathLiteral" .= (makeJsStringLiteral . AS.Route.path . AS.resolveRef spec <$> AS.Mcp.confirmRoute mcp),
          "insufficientScopeJson" .= (encodeJson <$> AS.Mcp.insufficientScope mcp),
          "initialScopesLiteral" .= (encodeJson <$> AS.Mcp.initialScopes mcp),
          "rateLimitLiteral" .= (rateLimitLiteral <$> AS.Mcp.rateLimit mcp),
          "mcpSecretEnvVarName" .= mcpSecretEnvVarName
        ]

    consentRoute = AS.resolveRef spec (AS.Mcp.consentRoute mcp)
    userEntity = AS.resolveRef spec (AS.Auth.userEntity auth)

    -- The lib passes user IDs as strings; Prisma wants the user entity's ID
    -- type (String, Int or BigInt: Valid.Mcp checks it).
    userIdFromString var = case Psl.Model._type <$> AS.Entity.getIdField userEntity of
      Just Psl.Model.Int -> "Number(" ++ var ++ ")"
      Just Psl.Model.BigInt -> "BigInt(" ++ var ++ ")"
      _ -> var

toolData :: (AS.Operation.Operation, AS.Expose.McpTool) -> Aeson.Value
toolData (op, tool) =
  object
    [ "operationName" .= operationName,
      "nameLiteral" .= makeJsStringLiteral (AS.Operation.getMcpToolName op tool),
      "titleLiteral" .= (makeJsStringLiteral <$> AS.Expose.title tool),
      "scopeLiteral" .= (makeJsStringLiteral <$> AS.Expose.scope tool),
      "insufficientScopeJson" .= (encodeJson <$> AS.Expose.insufficientScope tool),
      "descriptionLiteral" .= makeJsStringLiteral (AS.Expose.description tool),
      "input" .= schemaImport "Input" (Just $ AS.Expose.input tool),
      "output" .= schemaImport "Output" (AS.Expose.output tool),
      -- Queries are read-only; actions aren't.
      "readOnly" .= jsBool (AS.Operation.isQuery op),
      "idempotent" .= jsBool (fromMaybe False $ AS.Expose.idempotent tool),
      "openWorld" .= jsBool (fromMaybe False $ AS.Expose.openWorld tool),
      "destructive" .= jsBool (AS.Expose.isDestructive tool),
      "describe" .= schemaImport "Describe" (AS.Expose.describe tool)
    ]
  where
    operationName = AS.Operation.getName op
    -- Aliased, so schemas never clash with each other or with the operations.
    schemaImport suffix =
      extImportToAliasedImportJson ("_waspMcp" ++ suffix ++ "_" ++ operationName) relPathFromMcpDirToSrcDir

relPathFromMcpDirToSrcDir :: Path Posix (Rel ()) (Dir C.ServerSrcDir)
relPathFromMcpDirToSrcDir = [reldirP|..|]

-- | The lib wants both numbers, so the defaults are filled in here.
rateLimitLiteral :: AS.Mcp.McpRateLimit -> String
rateLimitLiteral limit
  | not (AS.Mcp.enabled limit) = "false"
  | otherwise =
      "{ windowMs: "
        ++ show (fromMaybe 60000 $ AS.Mcp.windowMs limit)
        ++ ", max: "
        ++ show (fromMaybe 120 $ AS.Mcp.max limit)
        ++ " }"

-- | A JS object literal of the scopes, keeping their declaration order (the
-- consent screen's checkbox order), which an Aeson object wouldn't.
scopesLiteral :: [AS.Mcp.McpScope] -> String
scopesLiteral [] = "{}"
scopesLiteral scopes = "{\n" ++ concatMap scopeEntry scopes ++ "}"
  where
    scopeEntry scope =
      "  " ++ makeJsStringLiteral (AS.Mcp.name scope) ++ ": " ++ encodeJson (scopeJson scope) ++ ",\n"
    scopeJson scope =
      object $
        ["label" .= AS.Mcp.label scope]
          ++ ["description" .= d | Just d <- [AS.Mcp.description scope]]
          ++ ["implies" .= i | Just i <- [AS.Mcp.implies scope]]

encodeJson :: (Aeson.ToJSON a) => a -> String
encodeJson = TL.unpack . Aeson.Text.encodeToLazyText

jsBool :: Bool -> String
jsBool b = if b then "true" else "false"
