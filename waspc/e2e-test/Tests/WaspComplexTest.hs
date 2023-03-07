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

waspComplexTest :: GoldenTest
waspComplexTest = do
  makeGoldenTest "waspComplexTest" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject
      ]
      <++> addServerEnvFile
      <++> addDependencies
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
          "GOOGLE_CLIENT_SECRET=google_client_secret"
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

insertCodeIntoWaspFileAfterVersion :: String -> ShellCommandBuilder ShellCommand
insertCodeIntoWaspFileAfterVersion = insertCodeIntoWaspFileAtLineNumber lineNumberInWaspFileAfterVersion
  where
    lineNumberInWaspFileAfterVersion :: Int
    lineNumberInWaspFileAfterVersion = 5

addApi :: ShellCommandBuilder [ShellCommand]
addApi = do
  sequence
    [ appendToWaspFile apiDecl,
      createFile apiFile "./src/server" "apis.ts"
    ]
  where
    apiDecl =
      unlines
        [ "api fooBar {",
          "  fn: import { fooBar } from \"@server/apis.js\",",
          "  httpRoute: (GET, \"/foo/bar\")",
          "}"
        ]

    apiFile =
      unlines
        [ "import { Request, Response } from '@wasp/types'",
          "import { FooBarContext } from '@wasp/apis/types'",
          "export function fooBar(req: Request, res: Response, context: FooBarContext) {",
          "  res.set('Access-Control-Allow-Origin', '*')",
          "  res.json({ msg: 'Hello, world!' })",
          "}"
        ]
