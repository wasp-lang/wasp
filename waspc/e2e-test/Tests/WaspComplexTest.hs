module Tests.WaspComplexTest (waspComplexTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToPrismaFile,
    appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    insertCodeIntoFileAtLineNumber,
    setDbToPSQL,
    waspCliCompile,
    waspCliNewMinimalStarter,
  )
import Util ((<++>))
import Wasp.Project.Db (databaseUrlEnvVarName)

waspComplexTest :: GoldenTest
waspComplexTest = do
  makeGoldenTest "waspComplexTest" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject
      ]
      <++> addServerEnvFile
      <++> addDependencies
      <++> addEmailSender
      <++> addClientSetup
      <++> addServerSetup
      <++> addAuth
      <++> sequence
        [ -- Prerequisite for jobs
          setDbToPSQL
        ]
      <++> addJob
      <++> addTsJob
      <++> addAction
      <++> addQuery
      <++> addApi
      <++> addApiNamespace
      <++> addCrud
      <++> sequence
        [ waspCliCompile
        ]

addClientSetup :: ShellCommandBuilder [ShellCommand]
addClientSetup = do
  sequence
    [ createFile rootComponentContent "./src/client" "App.jsx",
      createFile clientSetupFnContent "./src/client" "myClientSetupCode.js",
      insertCodeIntoWaspFileAfterVersion clientField
    ]
  where
    clientField =
      unlines
        [ "  client: {",
          "    setupFn: import myClientSetupFunction from \"@src/client/myClientSetupCode.js\",",
          "    rootComponent: import App from \"@src/client/App.jsx\"",
          "  },"
        ]
    rootComponentContent =
      unlines
        [ "export default function App({ children }) {",
          "  return (",
          "    <div className=\"app\">",
          "      <h1>Root component</h1>",
          "      {children}",
          "    </div>",
          "  );",
          "}"
        ]

    clientSetupFnContent =
      unlines
        [ "export default function myClientSetupFunction() {",
          "  // Do some client setup here.",
          "}"
        ]

addServerSetup :: ShellCommandBuilder [ShellCommand]
addServerSetup = do
  sequence
    [ createFile serverSetupFnContent "./src/server" "myServerSetupCode.js",
      insertCodeIntoWaspFileAfterVersion serverField
    ]
  where
    serverField =
      unlines
        [ "  server: {",
          "    setupFn: import mySetupFunction from \"@src/server/myServerSetupCode.js\",",
          "  },"
        ]
    serverSetupFnContent =
      unlines
        [ "export default function mySetupFunction() {",
          "  // Do some server setup here.",
          "}"
        ]

addJob :: ShellCommandBuilder [ShellCommand]
addJob = do
  sequence
    [ appendToWaspFile jobDecl,
      createFile jobFile "./src/server/jobs" "bar.js"
    ]
  where
    jobDecl =
      unlines
        [ "job mySpecialJob {",
          "  executor: PgBoss,",
          "  perform: {",
          "    fn: import { foo } from \"@src/server/jobs/bar.js\"",
          "  }",
          "}"
        ]

    jobFile =
      unlines
        [ "export const foo = async (args) => {",
          "  return 1",
          "}"
        ]

addTsJob :: ShellCommandBuilder [ShellCommand]
addTsJob = do
  sequence
    [ appendToWaspFile jobDecl,
      createFile jobFile "./src/server/jobs" "returnHello.ts"
    ]
  where
    jobDecl =
      unlines
        [ "job returnHelloJob {",
          "  executor: PgBoss,",
          "  perform: {",
          "    fn: import { returnHello } from \"@src/server/jobs/returnHello.js\",",
          "  },",
          "  entities: [User],",
          "}"
        ]

    jobFile =
      unlines
        [ "import { ReturnHelloJob } from 'wasp/server/jobs'",
          "export const returnHello: ReturnHelloJob<{ name: string }, string> = async (args) => {",
          "  return args.name",
          "}"
        ]

addServerEnvFile :: ShellCommandBuilder [ShellCommand]
addServerEnvFile = do
  sequence [createFile envFileContents "./" ".env.server"]
  where
    envFileContents =
      unlines
        [ "GOOGLE_CLIENT_ID=google_client_id",
          "GOOGLE_CLIENT_SECRET=google_client_secret",
          -- NOTE: Since we are using PSQL in this test, if we don't set custom
          -- database url in server/.env, Wasp will set its own, for managed dev db.
          -- That is problematic because Wasp's db url depends on project's abs path,
          -- which is not something we have constant during e2e tests, it depends
          -- on the location where the tests are being run.
          -- Therefore, we make sure to set custom database url here, to avoid .env
          -- changing between different machines / setups.
          databaseUrlEnvVarName <> "=" <> "mock-database-url",
          "SENDGRID_API_KEY=sendgrid_api_key"
        ]

-- Adds Google Auth with auth hooks
addAuth :: ShellCommandBuilder [ShellCommand]
addAuth = do
  sequence
    [ insertCodeIntoWaspFileAfterVersion authField,
      appendToPrismaFile userModel,
      createFile hooksFile "./src/auth" "hooks.ts"
    ]
  where
    authField =
      unlines
        [ "  auth: {",
          "    userEntity: User,",
          "    methods: {",
          "      google: {}",
          "    },",
          "    onAuthFailedRedirectTo: \"/login\",",
          "    onBeforeSignup: import { onBeforeSignup } from \"@src/auth/hooks.js\",",
          "    onAfterSignup: import { onAfterSignup } from \"@src/auth/hooks.js\",",
          "    onBeforeOAuthRedirect: import { onBeforeOAuthRedirect } from \"@src/auth/hooks.js\",",
          "  },"
        ]

    userModel =
      unlines
        [ "model User {",
          "  id          Int     @id @default(autoincrement())",
          "}"
        ]
    hooksFile =
      unlines
        [ "import type {",
          "  OnAfterSignupHook,",
          "  OnBeforeOAuthRedirectHook,",
          "  OnBeforeSignupHook,",
          "} from 'wasp/server/auth'",
          "",
          "export const onBeforeSignup: OnBeforeSignupHook = async (args) => {",
          "  const count = await args.prisma.user.count()",
          "  console.log('before', count)",
          "  console.log(args.providerId)",
          "}",
          "",
          "export const onAfterSignup: OnAfterSignupHook = async (args) => {",
          "  const count = await args.prisma.user.count()",
          "  console.log('after', count)",
          "  console.log('user', args.user)",
          "}",
          "",
          "export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async (",
          "  args,",
          ") => {",
          "  console.log('redirect to', args.url.toString())",
          "  return { url: args.url }",
          "}"
        ]

addAction :: ShellCommandBuilder [ShellCommand]
addAction = do
  sequence
    [ appendToWaspFile actionDecl,
      createFile actionFile "./src/server/actions" "bar.js"
    ]
  where
    actionDecl =
      unlines
        [ "action mySpecialAction {",
          "  fn: import { foo } from \"@src/server/actions/bar.js\",",
          "  entities: [User],",
          "}"
        ]

    actionFile =
      unlines
        [ "export const foo = async (args) => {",
          "  return 1",
          "}"
        ]

addQuery :: ShellCommandBuilder [ShellCommand]
addQuery = do
  sequence
    [ appendToWaspFile queryDecl,
      createFile queryFile "./src/server/queries" "bar.js"
    ]
  where
    queryDecl =
      unlines
        [ "query mySpecialQuery {",
          "  fn: import { foo } from \"@src/server/queries/bar.js\",",
          "  entities: [User],",
          "}"
        ]

    queryFile =
      unlines
        [ "export const foo = async (args) => {",
          "  return 1",
          "}"
        ]

addDependencies :: ShellCommandBuilder [ShellCommand]
addDependencies = do
  sequence
    [ insertCodeIntoPackageJsonIntoDependencies deps
    ]
  where
    deps =
      unlines
        [ "    \"redux\": \"^4.0.5\",",
          "    \"react-redux\": \"^7.1.3\","
        ]

addApi :: ShellCommandBuilder [ShellCommand]
addApi = do
  sequence
    [ appendToWaspFile apiDecls,
      createFile apiFile "./src/server" "apis.ts"
    ]
  where
    apiDecls =
      unlines
        [ "api fooBar {",
          "  fn: import { fooBar } from \"@src/server/apis.js\",",
          "  httpRoute: (GET, \"/foo/bar\"),",
          "  middlewareConfigFn: import { fooBarMiddlewareFn } from \"@src/server/apis.js\"",
          "}",
          "api fooBaz {",
          "  fn: import { fooBaz } from \"@src/server/apis.js\",",
          "  httpRoute: (GET, \"/foo/baz\"),",
          "  auth: false",
          "}"
        ]

    apiFile =
      unlines
        [ "import { FooBar, FooBaz } from 'wasp/server/api'",
          "import { MiddlewareConfigFn } from 'wasp/server'",
          "export const fooBar: FooBar = (req, res, context) => {",
          "  res.set('Access-Control-Allow-Origin', '*')",
          "  res.json({ msg: 'Hello, context.user.username!' })",
          "}",
          "export const fooBaz: FooBaz = (req, res, context) => {",
          "  res.json({ msg: 'Hello, stranger!' })",
          "}",
          "export const fooBarMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {",
          "  return middlewareConfig",
          "}"
        ]

addApiNamespace :: ShellCommandBuilder [ShellCommand]
addApiNamespace = do
  sequence
    [ appendToWaspFile apiNamespaceDecl,
      createFile apiNamespaceFile "./src/server" "apiNamespaces.ts"
    ]
  where
    apiNamespaceDecl =
      unlines
        [ "apiNamespace fooBarNamespace {",
          "  middlewareConfigFn: import { fooBarNamespaceMiddlewareFn } from \"@src/server/apiNamespaces.js\",",
          "  path: \"/bar\"",
          "}"
        ]

    apiNamespaceFile =
      unlines
        [ "import { MiddlewareConfigFn } from 'wasp/server'",
          "export const fooBarNamespaceMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {",
          "  return middlewareConfig",
          "}"
        ]

addEmailSender :: ShellCommandBuilder [ShellCommand]
addEmailSender = do
  sequence
    [ insertCodeIntoWaspFileAfterVersion emailSender
    ]
  where
    emailSender =
      unlines
        [ "  emailSender: {",
          "    provider: SendGrid,",
          "    defaultFrom: {",
          "      name: \"Hello\",",
          "      email: \"hello@itsme.com\"",
          "    },",
          "  },"
        ]

addCrud :: ShellCommandBuilder [ShellCommand]
addCrud = do
  sequence
    [ appendToPrismaFile taskModel,
      appendToWaspFile crudDecl
    ]
  where
    taskModel =
      unlines
        [ "model Task {",
          "  id          Int     @id @default(autoincrement())",
          "  description String",
          "  isDone      Boolean @default(false)",
          "}"
        ]

    crudDecl =
      unlines
        [ "crud tasks {",
          "  entity: Task,",
          "  operations: {",
          "    get: {},",
          "    getAll: {},",
          "    create: {},",
          "  }",
          "}"
        ]

insertCodeIntoWaspFileAfterVersion :: String -> ShellCommandBuilder ShellCommand
insertCodeIntoWaspFileAfterVersion = insertCodeIntoFileAtLineNumber "main.wasp" lineNumberInWaspFileAfterVersion
  where
    lineNumberInWaspFileAfterVersion :: Int
    lineNumberInWaspFileAfterVersion = 5

insertCodeIntoPackageJsonIntoDependencies :: String -> ShellCommandBuilder ShellCommand
insertCodeIntoPackageJsonIntoDependencies = insertCodeIntoFileAtLineNumber "package.json" lineNumberInPackageJsonDependencies
  where
    lineNumberInPackageJsonDependencies :: Int
    lineNumberInPackageJsonDependencies = 3
