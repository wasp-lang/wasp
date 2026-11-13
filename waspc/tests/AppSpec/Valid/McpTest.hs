module AppSpec.Valid.McpTest where

import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Fixtures.AppSpec (basicAppSpec)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Mcp as AS.Mcp
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.Expose as AS.Expose
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import qualified Wasp.Valid as Valid
import qualified Wasp.Version as WV

spec_McpValidation :: Spec
spec_McpValidation = do
  it "accepts a valid MCP setup" $ do
    validate defaultSetup `shouldBe` []

  it "accepts operations without expose.mcp when app.mcp is off" $ do
    validate defaultSetup {setupMcp = Nothing, setupTools = []} `shouldBe` []

  it "requires app.mcp for expose.mcp" $ do
    validate defaultSetup {setupMcp = Nothing}
      `shouldHaveErrorContaining` "The query 'getTasks' sets expose.mcp, which requires app.mcp."

  it "requires auth for app.mcp" $ do
    validate defaultSetup {setupAuth = False}
      `shouldHaveErrorContaining` "app.mcp requires app.auth"

  it "refuses expose.mcp on an operation with auth: false" $ do
    validate defaultSetup {setupQueryAuth = Just False}
      `shouldHaveErrorContaining` "has auth: false"

  it "requires the consent page to need auth" $ do
    validate defaultSetup {setupConsentAuthRequired = Nothing}
      `shouldHaveErrorContaining` "must point to a page with authRequired: true"

  it "requires non-empty instructions and descriptions" $ do
    validate defaultSetup {setupInstructions = "  "}
      `shouldHaveErrorContaining` "app.mcp.instructions can't be empty"
    validate defaultSetup {setupTools = [tool {AS.Expose.description = ""}]}
      `shouldHaveErrorContaining` "needs a description"

  it "checks tool names" $ do
    validate defaultSetup {setupTools = [tool {AS.Expose.name = Just "ListTasks"}]}
      `shouldHaveErrorContaining` "must match ^[a-z][a-z0-9_]{0,63}$"
    validate defaultSetup {setupTools = [tool {AS.Expose.name = Just "list_tasks"}, tool {AS.Expose.name = Just "list_tasks"}]}
      `shouldHaveErrorContaining` "More than one operation is exposed as the MCP tool 'list_tasks'"
    validate defaultSetup {setupTools = [tool {AS.Expose.name = Just "whoami"}]}
      `shouldHaveErrorContaining` "taken by the built-in tool"
    validate defaultSetup {setupTools = [tool {AS.Expose.name = Just "whoami"}], setupWhoami = Just False}
      `shouldBe` []

  it "names tools after their operation by default" $ do
    -- getTasks and getTasks2 become get_tasks and get_tasks2.
    validate defaultSetup {setupTools = [tool, tool]} `shouldBe` []
    -- An explicit name that matches another tool's default clashes with it.
    validate defaultSetup {setupTools = [tool, tool {AS.Expose.name = Just "get_tasks"}]}
      `shouldHaveErrorContaining` "More than one operation is exposed as the MCP tool 'get_tasks'"

  it "refuses destructive on a query, since queries are read-only" $ do
    validate defaultSetup {setupTools = [tool {AS.Expose.destructive = Just True}]}
      `shouldHaveErrorContaining` "The query 'getTasks' sets expose.mcp.destructive, but queries are read-only."

  it "accepts declared scopes and tools that use them" $ do
    validate defaultSetup {setupScopes = Just validScopes, setupTools = [tool {AS.Expose.scope = Just "tasks:read"}]}
      `shouldBe` []

  it "requires a tool's scope to be declared" $ do
    validate defaultSetup {setupTools = [tool {AS.Expose.scope = Just "tasks:read"}]}
      `shouldHaveErrorContaining` "needs the scope 'tasks:read', which isn't declared in app.mcp.scopes."

  it "checks scope names, labels and implies" $ do
    let withScopes scopes = validate defaultSetup {setupScopes = Just scopes}
    withScopes [scope "tasks read" "View"]
      `shouldHaveErrorContaining` "can't be empty or contain spaces, quotes or backslashes."
    withScopes [scope "tasks:read" " "]
      `shouldHaveErrorContaining` "The MCP scope 'tasks:read' needs a label"
    withScopes [(scope "tasks:write" "Edit") {AS.Mcp.implies = Just ["tasks:read"]}]
      `shouldHaveErrorContaining` "implies 'tasks:read', which isn't declared"
    withScopes
      [ (scope "a" "A") {AS.Mcp.implies = Just ["b"]},
        (scope "b" "B") {AS.Mcp.implies = Just ["a"]}
      ]
      `shouldHaveErrorContaining` "The MCP scope 'a' implies itself"

  it "requires confirmRoute for destructive tools, once however many there are" $ do
    let errors = validate defaultSetup {setupActionTools = [tool {AS.Expose.destructive = Just True}, tool {AS.Expose.destructive = Just True}]}
    filter (("set app.mcp.confirmRoute" `isInfixOf`) . show) errors `shouldSatisfy` ((== 1) . length)

  it "accepts a destructive tool with a confirm route" $ do
    validate defaultSetup {setupActionTools = [tool {AS.Expose.destructive = Just True}], setupConfirm = True}
      `shouldBe` []

  it "requires the confirm page to need auth" $ do
    validate defaultSetup {setupActionTools = [tool {AS.Expose.destructive = Just True}], setupConfirm = True, setupConfirmAuthRequired = Nothing}
      `shouldHaveErrorContaining` "(app.mcp.confirmRoute) must point to a page with authRequired: true"

  it "checks initialScopes against the declared scopes" $ do
    let withInitial initial = validate defaultSetup {setupScopes = Just validScopes, setupMcpOverrides = \m -> m {AS.Mcp.initialScopes = Just initial}}
    withInitial ["tasks:read"] `shouldBe` []
    withInitial [] `shouldHaveErrorContaining` "app.mcp.initialScopes can't be empty"
    withInitial ["tasks:delete"] `shouldHaveErrorContaining` "app.mcp.initialScopes has 'tasks:delete', which isn't declared"

  it "requires a scope for a tool's insufficientScope" $ do
    validate defaultSetup {setupTools = [tool {AS.Expose.insufficientScope = Just AS.Mcp.Hide}]}
      `shouldHaveErrorContaining` "sets insufficientScope, which only applies to a tool with a scope"
    validate defaultSetup {setupScopes = Just validScopes, setupTools = [tool {AS.Expose.scope = Just "tasks:read", AS.Expose.insufficientScope = Just AS.Mcp.Hide}]}
      `shouldBe` []

  it "checks the rate limit" $ do
    validate defaultSetup {setupMcpOverrides = \m -> m {AS.Mcp.rateLimit = Just (AS.Mcp.McpRateLimit True (Just 60000) (Just 0))}}
      `shouldHaveErrorContaining` "app.mcp.rateLimit.max must be a positive whole number."

  it "reserves the names of the entities Wasp adds" $ do
    validate defaultSetup {setupExtraModels = "model McpConnection {\n  id String @id\n}"}
      `shouldHaveErrorContaining` "The entity name 'McpConnection' is reserved"
    validate defaultSetup {setupUserExtraFields = "  mcpConnections String"}
      `shouldHaveErrorContaining` "can't have a field named 'mcpConnections'"

  it "supports String, Int and BigInt user ids" $ do
    validate defaultSetup {setupUserIdType = "String @default(uuid())"} `shouldBe` []
    validate defaultSetup {setupUserIdType = "BigInt @default(autoincrement())"} `shouldBe` []
    validate defaultSetup {setupUserIdType = "DateTime @default(now())"}
      `shouldHaveErrorContaining` "to have an id of type String, Int or BigInt"

  it "refuses consent and confirm routes with parameters" $ do
    validate defaultSetup {setupConsentPath = "/connect/:client"}
      `shouldHaveErrorContaining` "(app.mcp.consentRoute) can't have parameters in its path"

  it "warns that app.mcp is experimental" $ do
    warnings defaultSetup `shouldBe` ["app.mcp is experimental: its API may change in any Wasp release."]
    warnings defaultSetup {setupMcp = Nothing, setupTools = []} `shouldBe` []
  where
    tool =
      AS.Expose.McpTool
        { AS.Expose.name = Nothing,
          AS.Expose.title = Nothing,
          AS.Expose.scope = Nothing,
          AS.Expose.insufficientScope = Nothing,
          AS.Expose.description = "Lists the user's tasks.",
          AS.Expose.input = dummyExtImport,
          AS.Expose.output = Nothing,
          AS.Expose.idempotent = Nothing,
          AS.Expose.openWorld = Nothing,
          AS.Expose.destructive = Nothing,
          AS.Expose.describe = Nothing
        }
    defaultSetup =
      Setup
        { setupMcp = Just (),
          setupAuth = True,
          setupInstructions = "Tasks keeps a to-do list. Start with list_tasks.",
          setupWhoami = Nothing,
          setupConsentAuthRequired = Just True,
          setupConsentPath = "/connect",
          setupTools = [tool],
          setupQueryAuth = Nothing,
          setupUserIdType = "Int @default(autoincrement())",
          setupUserExtraFields = "",
          setupScopes = Nothing,
          setupActionTools = [],
          setupConfirm = False,
          setupConfirmAuthRequired = Just True,
          setupMcpOverrides = id,
          setupExtraModels = ""
        }

data Setup = Setup
  { setupMcp :: Maybe (),
    setupAuth :: Bool,
    setupInstructions :: String,
    setupWhoami :: Maybe Bool,
    setupConsentAuthRequired :: Maybe Bool,
    setupConsentPath :: String,
    -- | One query per tool, named getTasks, getTasks2, ...
    setupTools :: [AS.Expose.McpTool],
    setupQueryAuth :: Maybe Bool,
    setupUserIdType :: String,
    -- | More lines for the user model.
    setupUserExtraFields :: String,
    setupScopes :: Maybe [AS.Mcp.McpScope],
    -- | One action per tool, named deleteTask, deleteTask2, ...
    setupActionTools :: [AS.Expose.McpTool],
    -- | Sets confirmRoute and declares its route and page.
    setupConfirm :: Bool,
    setupConfirmAuthRequired :: Maybe Bool,
    -- | Sets the app.mcp fields the other setup fields don't cover.
    setupMcpOverrides :: AS.Mcp.Mcp -> AS.Mcp.Mcp,
    -- | More models for schema.prisma.
    setupExtraModels :: String
  }

scope :: String -> String -> AS.Mcp.McpScope
scope name label = AS.Mcp.McpScope {AS.Mcp.name = name, AS.Mcp.label = label, AS.Mcp.description = Nothing, AS.Mcp.implies = Nothing}

validScopes :: [AS.Mcp.McpScope]
validScopes =
  [ scope "tasks:read" "View your tasks",
    (scope "tasks:write" "Edit your tasks") {AS.Mcp.implies = Just ["tasks:read"]}
  ]

-- | The errors, without warnings.
validate :: Setup -> [Valid.ValidationError]
validate setup = filter Valid.isValidationError $ ASV.validateAppSpec (makeSpec setup)

warnings :: Setup -> [String]
warnings setup = [message | Valid.GenericValidationWarning message <- ASV.validateAppSpec (makeSpec setup)]

shouldHaveErrorContaining :: [Valid.ValidationError] -> String -> Expectation
shouldHaveErrorContaining errors text =
  errors `shouldSatisfy` any containsText
  where
    containsText (Valid.GenericValidationError message) = text `isInfixOf` message
    containsText (Valid.GenericValidationWarning _) = False

makeSpec :: Setup -> AS.AppSpec
makeSpec setup =
  basicAppSpec
    { AS.decls =
        [AS.Decl.makeDecl app, consentPageDecl, consentRouteDecl, homePageDecl, homeRouteDecl]
          ++ (if setupConfirm setup then [confirmPageDecl, confirmRouteDecl] else [])
          ++ zipWith queryDecl [1 :: Int ..] (setupTools setup)
          ++ zipWith actionDecl [1 :: Int ..] (setupActionTools setup)
          ++ entityDecls,
      AS.prismaSchema = schema
    }
  where
    schema =
      Util.getPrismaSchema
        $ T.pack
        $ unlines
          [ "datasource db {",
            "  provider = \"postgresql\"",
            "  url = env(\"DATABASE_URL\")",
            "}",
            "generator client {",
            "  provider = \"prisma-client-js\"",
            "}",
            "model User {",
            "  id             " ++ setupUserIdType setup ++ " @id",
            setupUserExtraFields setup,
            "}",
            setupExtraModels setup
          ]
    entityDecls = AS.Entity.makeEntityDecls schema

    app =
      AS.App.App
        { AS.App.name = "TestApp",
          AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = "^" ++ show WV.waspVersion},
          AS.App.title = "Test App",
          AS.App.deployment = Nothing,
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = if setupAuth setup then Just auth else Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing,
          AS.App.mcp =
            const
              ( setupMcpOverrides setup $
                  AS.Mcp.Mcp
                    { AS.Mcp.instructions = setupInstructions setup,
                      AS.Mcp.consentRoute = AS.Core.Ref.Ref "McpConsentRoute",
                      AS.Mcp.confirmRoute = if setupConfirm setup then Just (AS.Core.Ref.Ref "McpConfirmRoute") else Nothing,
                      AS.Mcp.whoami = setupWhoami setup,
                      AS.Mcp.scopes = setupScopes setup,
                      AS.Mcp.initialScopes = Nothing,
                      AS.Mcp.insufficientScope = Nothing,
                      AS.Mcp.rateLimit = Nothing
                    }
              )
              <$> setupMcp setup
        }

    auth =
      AS.Auth.Auth
        { AS.Auth.userEntity = AS.Core.Ref.Ref "User",
          AS.Auth.methods =
            AS.Auth.AuthMethods
              { AS.Auth.usernameAndPassword = Just AS.Auth.UsernameAndPasswordConfig {AS.Auth.userSignupFields = Nothing, AS.Auth.passwordValidationSchema = Nothing},
                AS.Auth.discord = Nothing,
                AS.Auth.slack = Nothing,
                AS.Auth.google = Nothing,
                AS.Auth.gitHub = Nothing,
                AS.Auth.keycloak = Nothing,
                AS.Auth.microsoft = Nothing,
                AS.Auth.email = Nothing
              },
          AS.Auth.onAuthFailedRedirectTo = "/",
          AS.Auth.onAuthSucceededRedirectTo = Nothing,
          AS.Auth.onBeforeSignup = Nothing,
          AS.Auth.onAfterSignup = Nothing,
          AS.Auth.onAfterEmailVerified = Nothing,
          AS.Auth.onBeforeOAuthRedirect = Nothing,
          AS.Auth.onBeforeLogin = Nothing,
          AS.Auth.onAfterLogin = Nothing
        }

    consentPageDecl =
      AS.Decl.makeDecl
        AS.Page.Page
          { AS.Page.name = "McpConsentPage",
            AS.Page.component = dummyExtImport,
            AS.Page.authRequired = setupConsentAuthRequired setup
          }
    consentRouteDecl =
      AS.Decl.makeDecl
        AS.Route.Route
          { AS.Route.name = "McpConsentRoute",
            AS.Route.to = AS.Core.Ref.Ref "McpConsentPage",
            AS.Route.path = setupConsentPath setup,
            AS.Route.lazy = Nothing,
            AS.Route.prerender = []
          }
    confirmPageDecl =
      AS.Decl.makeDecl
        AS.Page.Page {AS.Page.name = "McpConfirmPage", AS.Page.component = dummyExtImport, AS.Page.authRequired = setupConfirmAuthRequired setup}
    confirmRouteDecl =
      AS.Decl.makeDecl
        AS.Route.Route {AS.Route.name = "McpConfirmRoute", AS.Route.to = AS.Core.Ref.Ref "McpConfirmPage", AS.Route.path = "/confirm", AS.Route.lazy = Nothing, AS.Route.prerender = []}
    actionDecl index mcpTool =
      AS.Decl.makeDecl
        AS.Action.Action
          { AS.Action.name = "deleteTask" ++ (if index == 1 then "" else show index),
            AS.Action.fn = dummyExtImport,
            AS.Action.entities = Nothing,
            AS.Action.auth = Nothing,
            AS.Action.expose = Just AS.Expose.Expose {AS.Expose.client = Nothing, AS.Expose.mcp = Just mcpTool}
          }
    homePageDecl =
      AS.Decl.makeDecl
        AS.Page.Page {AS.Page.name = "HomePage", AS.Page.component = dummyExtImport, AS.Page.authRequired = Nothing}
    homeRouteDecl =
      AS.Decl.makeDecl
        AS.Route.Route {AS.Route.name = "HomeRoute", AS.Route.to = AS.Core.Ref.Ref "HomePage", AS.Route.path = "/", AS.Route.lazy = Nothing, AS.Route.prerender = []}

    queryDecl index mcpTool =
      AS.Decl.makeDecl
        AS.Query.Query
          { AS.Query.name = "getTasks" ++ (if index == 1 then "" else show index),
            AS.Query.fn = dummyExtImport,
            AS.Query.entities = Nothing,
            AS.Query.auth = setupQueryAuth setup,
            AS.Query.expose = Just AS.Expose.Expose {AS.Expose.client = Nothing, AS.Expose.mcp = Just mcpTool}
          }

dummyExtImport :: AS.ExtImport.ExtImport
dummyExtImport =
  AS.ExtImport.ExtImport
    (AS.ExtImport.ExtImportModule "Dummy")
    (fromJust $ SP.parseRelFileP "dummy/File")
    Nothing
