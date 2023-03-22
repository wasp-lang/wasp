module Tests.WaspComplexTest (waspComplexTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    insertCodeIntoWaspFileAtLineNumber,
    setDbToPSQL,
    waspCliCompile,
    waspCliNew,
  )
import Util ((<++>))
import Wasp.Project.Db (databaseUrlEnvVarName)

waspComplexTest :: GoldenTest
waspComplexTest = do
  makeGoldenTest "waspComplexTest" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject
      ]
      <++> addServerEnvFile
      <++> addDependencies
      <++> addEmailSender
      <++> addClientSetup
      <++> addServerSetup
      <++> addGoogleAuth
      <++> addJob
      <++> addAction
      <++> addQuery
      <++> addApi
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
          "    setupFn: import myClientSetupFunction from \"@client/myClientSetupCode.js\",",
          "    rootComponent: import App from \"@client/App.jsx\"",
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
          "    setupFn: import mySetupFunction from \"@server/myServerSetupCode.js\",",
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
    [ setDbToPSQL,
      appendToWaspFile jobDecl,
      createFile jobFile "./src/server/jobs" "bar.js"
    ]
  where
    jobDecl =
      unlines
        [ "job MySpecialJob {",
          "  executor: PgBoss,",
          "  perform: {",
          "    fn: import { foo } from \"@server/jobs/bar.js\"",
          "  }",
          "}"
        ]

    jobFile =
      unlines
        [ "export const foo = async (args) => {",
          "  return 1",
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

addGoogleAuth :: ShellCommandBuilder [ShellCommand]
addGoogleAuth = do
  sequence
    [ insertCodeIntoWaspFileAfterVersion authField,
      appendToWaspFile userEntity,
      appendToWaspFile socialLoginEntity
    ]
  where
    authField =
      unlines
        [ "  auth: {",
          "    userEntity: User,",
          "    externalAuthEntity: SocialLogin,",
          "    methods: {",
          "      google: {}",
          "    },",
          "    onAuthFailedRedirectTo: \"/login\"",
          "  },"
        ]

    userEntity =
      unlines
        [ "entity User {=psl",
          "  id                        Int           @id @default(autoincrement())",
          "  username                  String        @unique",
          "  password                  String",
          "  externalAuthAssociations  SocialLogin[]",
          "psl=}"
        ]

    socialLoginEntity =
      unlines
        [ "entity SocialLogin {=psl",
          "  id          Int       @id @default(autoincrement())",
          "  provider    String",
          "  providerId  String",
          "  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)",
          "  userId      Int",
          "  createdAt   DateTime  @default(now())",
          "  @@unique([provider, providerId, userId])",
          "psl=}"
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
        [ "action MySpecialAction {",
          "  fn: import { foo } from \"@server/actions/bar.js\",",
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
        [ "query MySpecialQuery {",
          "  fn: import { foo } from \"@server/queries/bar.js\",",
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
    [ insertCodeIntoWaspFileAfterVersion deps
    ]
  where
    deps =
      unlines
        [ "  dependencies: [",
          "    (\"redux\", \"^4.0.5\"),",
          "    (\"react-redux\", \"^7.1.3\")",
          "  ],"
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
          "  fn: import { fooBar } from \"@server/apis.js\",",
          "  httpRoute: (GET, \"/foo/bar\")",
          "  // implicit auth:true",
          "}",
          "api fooBaz {",
          "  fn: import { fooBaz } from \"@server/apis.js\",",
          "  httpRoute: (GET, \"/foo/baz\"),",
          "  auth: false",
          "}"
        ]

    apiFile =
      unlines
        [ "import { FooBar, FooBaz } from '@wasp/apis/types'",
          "export const fooBar: FooBar = (req, res, context) => {",
          "  res.set('Access-Control-Allow-Origin', '*')",
          "  res.json({ msg: 'Hello, context.user.username!' })",
          "}",
          "export const fooBaz: FooBaz = (req, res, context) => {",
          "  res.json({ msg: 'Hello, stranger!' })",
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

insertCodeIntoWaspFileAfterVersion :: String -> ShellCommandBuilder ShellCommand
insertCodeIntoWaspFileAfterVersion = insertCodeIntoWaspFileAtLineNumber lineNumberInWaspFileAfterVersion
  where
    lineNumberInWaspFileAfterVersion :: Int
    lineNumberInWaspFileAfterVersion = 5