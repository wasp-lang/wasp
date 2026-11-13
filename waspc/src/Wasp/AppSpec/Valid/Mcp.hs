-- | Consistency checks for the experimental MCP feature (`app.mcp` and
-- `expose` on operations).
module Wasp.AppSpec.Valid.Mcp
  ( validateMcp,
  )
where

import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import Wasp.AppSpec.App.Mcp (Mcp)
import qualified Wasp.AppSpec.App.Mcp as Mcp
import Wasp.AppSpec.Core.Ref (Ref)
import qualified Wasp.AppSpec.Entity as Entity
import Wasp.AppSpec.Expose (McpTool)
import qualified Wasp.AppSpec.Expose as Expose
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Route as Route
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Util (findDuplicateElems)
import Wasp.Valid (ValidationError (..))

-- | MCP tool names are part of the contract with models.
mcpToolNamePattern :: String
mcpToolNamePattern = "^[a-z][a-z0-9_]{0,63}$"

validateMcp :: AppSpec -> App -> [ValidationError]
validateMcp spec app = case App.mcp app of
  Nothing ->
    [ GenericValidationError $
        describeOperation op
          ++ " sets expose.mcp, which requires app.mcp. Add app.mcp to your app, or remove expose.mcp."
    | (op, _) <- exposedTools
    ]
  Just mcp ->
    concat
      [ [GenericValidationError "app.mcp requires app.auth: every MCP tool runs as a logged-in user." | not hasAuth],
        validateInstructions mcp,
        validateConsentRoute spec mcp,
        validateScopes mcp,
        validateInitialScopes mcp,
        validateRateLimit mcp,
        validateConfirmations spec mcp exposedTools,
        validateToolNames mcp exposedTools,
        concatMap (uncurry (validateTool mcp)) exposedTools,
        maybe [] (validateEntitiesForMcp spec) (App.auth app),
        [GenericValidationWarning "app.mcp is experimental: its API may change in any Wasp release."]
      ]
  where
    hasAuth = isJust (App.auth app)
    exposedTools = AS.getMcpTools spec

validateInstructions :: Mcp -> [ValidationError]
validateInstructions mcp =
  [ GenericValidationError "app.mcp.instructions can't be empty: tell the model what the app is and which tool to start with."
  | isBlank (Mcp.instructions mcp)
  ]

validateConsentRoute :: AppSpec -> Mcp -> [ValidationError]
validateConsentRoute spec mcp =
  validateLoggedInRoute spec "app.mcp.consentRoute" "it shows the consent form to a logged-in user, and it's the OAuth authorization endpoint" (Mcp.consentRoute mcp)

-- | Destructive tools wait on the confirm page.
validateConfirmations :: AppSpec -> Mcp -> [(AS.Operation.Operation, McpTool)] -> [ValidationError]
validateConfirmations spec mcp tools =
  [ GenericValidationError $
      "The MCP tool '"
        ++ AS.Operation.getMcpToolName op tool
        ++ "' is destructive, so it waits for the user's approval: set app.mcp.confirmRoute (the page with <McpConfirmAction />)."
  | (op, tool) <- take 1 destructiveTools,
    isNothing (Mcp.confirmRoute mcp)
  ]
    ++ maybe [] (validateLoggedInRoute spec "app.mcp.confirmRoute" "it shows a destructive tool call to the logged-in user who approves it") (Mcp.confirmRoute mcp)
  where
    destructiveTools = filter (Expose.isDestructive . snd) tools

validateLoggedInRoute :: AppSpec -> String -> String -> Ref Route.Route -> [ValidationError]
validateLoggedInRoute spec fieldName why routeRef =
  [ GenericValidationError $
      "The route '"
        ++ Route.name route
        ++ "' ("
        ++ fieldName
        ++ ") must point to a page with authRequired: true, since "
        ++ why
        ++ "."
  | Page.authRequired page /= Just True
  ]
    ++ [ GenericValidationError $
           "The route '"
             ++ Route.name route
             ++ "' ("
             ++ fieldName
             ++ ") can't have parameters in its path, since Wasp links to it: use a path like /connect."
       | any (`elem` Route.path route) [':', '*', '?']
       ]
  where
    route = AS.resolveRef spec routeRef
    page = AS.resolveRef spec (Route.to route)

validateToolNames :: Mcp -> [(AS.Operation.Operation, McpTool)] -> [ValidationError]
validateToolNames mcp tools =
  [ GenericValidationError $
      "MCP tool name '"
        ++ toolName
        ++ "' ("
        ++ describeOperation op
        ++ ") must match "
        ++ mcpToolNamePattern
        ++ ", e.g. list_tasks. Without expose.mcp.name, it's the operation's name in snake_case."
  | (op, tool) <- tools,
    let toolName = AS.Operation.getMcpToolName op tool,
    not (toolName =~ mcpToolNamePattern)
  ]
    ++ [ GenericValidationError $
           "More than one operation is exposed as the MCP tool '" ++ duplicate ++ "'. Tool names must be unique."
       | duplicate <- findDuplicateElems toolNames
       ]
    ++ [ GenericValidationError
           "The MCP tool name 'whoami' is taken by the built-in tool. Rename your tool, or turn the built-in off with app.mcp.whoami: false."
       | Mcp.isWhoamiEnabled mcp,
         "whoami" `elem` toolNames
       ]
  where
    toolNames = map (uncurry AS.Operation.getMcpToolName) tools

-- | Scope names are OAuth scope tokens (RFC 6749 §3.3): no spaces, quotes or backslashes.
validateScopes :: Mcp -> [ValidationError]
validateScopes mcp =
  [ GenericValidationError $
      "The MCP scope name " ++ show name ++ " (app.mcp.scopes) can't be empty or contain spaces, quotes or backslashes."
  | name <- scopeNames,
    null name || any (`elem` (" \"\\" :: String)) name || any (\c -> c < '!' || c > '~') name
  ]
    ++ [ GenericValidationError $
           "The MCP scope '" ++ name ++ "' needs a label: it's what the consent screen shows."
       | scope <- scopes,
         let name = Mcp.name scope,
         isBlank (Mcp.label scope)
       ]
    ++ [ GenericValidationError $
           "The MCP scope '" ++ name ++ "' implies '" ++ implied ++ "', which isn't declared in app.mcp.scopes."
       | scope <- scopes,
         let name = Mcp.name scope,
         implied <- fromMaybe [] (Mcp.implies scope),
         implied `notElem` scopeNames
       ]
    ++ [ GenericValidationError $
           "The MCP scope '" ++ name ++ "' implies itself through `implies`. Scopes can't imply each other in a cycle."
       | name <- scopeNames,
         name `Set.member` impliedBy name
       ]
  where
    scopes = Mcp.getScopes mcp
    scopeNames = map Mcp.name scopes
    directlyImplied name = concat [fromMaybe [] (Mcp.implies s) | s <- scopes, Mcp.name s == name]
    -- Every scope reachable from `name` through `implies`, not counting `name` itself unless it's on a cycle.
    impliedBy name = go Set.empty (directlyImplied name)
      where
        go seen [] = seen
        go seen (next : rest)
          | next `Set.member` seen = go seen rest
          | otherwise = go (Set.insert next seen) (directlyImplied next ++ rest)

validateInitialScopes :: Mcp -> [ValidationError]
validateInitialScopes mcp = case Mcp.initialScopes mcp of
  Nothing -> []
  Just [] -> [GenericValidationError "app.mcp.initialScopes can't be empty: clients need at least one scope to connect. Leave it out to ask for every scope."]
  Just initial ->
    [ GenericValidationError $
        "app.mcp.initialScopes has '" ++ name ++ "', which isn't declared in app.mcp.scopes."
    | name <- initial,
      name `notElem` map Mcp.name (Mcp.getScopes mcp)
    ]

validateRateLimit :: Mcp -> [ValidationError]
validateRateLimit mcp =
  [ GenericValidationError $ "app.mcp.rateLimit." ++ field ++ " must be a positive whole number."
  | Just limit <- [Mcp.rateLimit mcp],
    (field, Just value) <- [("windowMs", Mcp.windowMs limit), ("max", Mcp.max limit)],
    value < 1
  ]

validateTool :: Mcp -> AS.Operation.Operation -> McpTool -> [ValidationError]
validateTool mcp op tool =
  [ GenericValidationError $
      describeOperation op
        ++ " sets expose.mcp but has auth: false. MCP tools always run as a logged-in user, so remove auth: false."
  | AS.Operation.getAuth op == Just False
  ]
    ++ [ GenericValidationError $
           "The MCP tool '" ++ toolName ++ "' needs a description, written for the model: what it does, what it returns, and which tool to use next."
       | isBlank (Expose.description tool)
       ]
    ++ [ GenericValidationError $
           "The MCP tool '"
             ++ toolName
             ++ "' needs the scope '"
             ++ scopeName
             ++ "', which isn't declared in app.mcp.scopes."
       | Just scopeName <- [Expose.scope tool],
         scopeName `notElem` map Mcp.name (Mcp.getScopes mcp)
       ]
    ++ [ GenericValidationError $
           describeOperation op
             ++ " sets expose.mcp.destructive, but queries are read-only. Make it an action if it changes data."
       | AS.Operation.isQuery op,
         Expose.isDestructive tool
       ]
    ++ [ GenericValidationError $
           "The MCP tool '"
             ++ toolName
             ++ "' sets insufficientScope, which only applies to a tool with a scope. Set expose.mcp.scope, or remove insufficientScope."
       | isJust (Expose.insufficientScope tool),
         isNothing (Expose.scope tool)
       ]
  where
    toolName = AS.Operation.getMcpToolName op tool

-- | Wasp injects the MCP entities (DbGenerator.Mcp), so their names and the
-- user entity's relation field to connections are taken, and the generated
-- code has to turn user ids from strings into the user entity's id type.
validateEntitiesForMcp :: AppSpec -> Auth.Auth -> [ValidationError]
validateEntitiesForMcp spec auth =
  [ GenericValidationError $
      "The entity name '" ++ name ++ "' is reserved: Wasp adds an entity with this name for app.mcp. Rename your entity."
  | name <- Mcp.mcpEntityNames,
    name `elem` map Entity.getName (AS.getEntities spec)
  ]
    ++ [ GenericValidationError $
           "The user entity '"
             ++ Entity.getName userEntity
             ++ "' can't have a field named '"
             ++ Mcp.mcpConnectionsFieldOnUserEntityName
             ++ "': Wasp adds it for app.mcp. Rename or remove the field."
       | any ((== Mcp.mcpConnectionsFieldOnUserEntityName) . Psl.Model._name) (Entity.getFields userEntity)
       ]
    ++ [ GenericValidationError $
           "app.mcp needs the user entity '"
             ++ Entity.getName userEntity
             ++ "' to have an id of type String, Int or BigInt."
       | (Psl.Model._type <$> Entity.getIdField userEntity) `notElem` map Just [Psl.Model.String, Psl.Model.Int, Psl.Model.BigInt]
       ]
  where
    userEntity = AS.resolveRef spec (Auth.userEntity auth)

describeOperation :: AS.Operation.Operation -> String
describeOperation op =
  (if AS.Operation.isQuery op then "The query '" else "The action '")
    ++ AS.Operation.getName op
    ++ "'"

isBlank :: String -> Bool
isBlank = all (`elem` [' ', '\t', '\n'])
