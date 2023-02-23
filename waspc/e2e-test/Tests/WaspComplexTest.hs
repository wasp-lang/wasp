module Tests.WaspComplexTest (waspComplexTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToWaspFile,
    cdIntoCurrentProject,
    createFile,
    insertLineAtWaspFile,
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
      <++> addDependencies
      <++> addClientSetup
      <++> addServerSetup
      <++> addGoogleAuth
      <++> addJob
      <++> addAction
      <++> addQuery
      <++> sequence
        [ waspCliCompile
        ]

addClientSetup :: ShellCommandBuilder [ShellCommand]
addClientSetup = do
  let rootComponentContent =
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

  let clientSetupFnContent =
        unlines
          [ "export default function myClientSetupFunction() {",
            "  // Do some client setup here.",
            "}"
          ]

  sequence
    [ createFile rootComponentContent "./src/client" "App.jsx",
      createFile clientSetupFnContent "./src/client" "myClientSetupCode.js",
      insertLineAtWaspFileAfterVersion "  client: { setupFn: import myClientSetupFunction from \"@client/myClientSetupCode.js\", rootComponent: import App from \"@client/App.jsx\" },"
    ]

addServerSetup :: ShellCommandBuilder [ShellCommand]
addServerSetup = do
  let serverSetupFnContent =
        unlines
          [ "export default function mySetupFunction() {",
            "  // Do some server setup here.",
            "}"
          ]

  sequence
    [ createFile serverSetupFnContent "./src/server" "myServerSetupCode.js",
      insertLineAtWaspFileAfterVersion "  server: { setupFn: import mySetupFunction from \"@server/myServerSetupCode.js\" },"
    ]

addJob :: ShellCommandBuilder [ShellCommand]
addJob = do
  let jobDecl =
        unlines
          [ "job MySpecialJob {",
            "  executor: PgBoss,",
            "  perform: {",
            "    fn: import { foo } from \"@server/jobs/bar.js\"",
            "  }",
            "}"
          ]

  let jobFile =
        unlines
          [ "export const foo = async (args) => {",
            "  return 1",
            "}"
          ]

  sequence
    [ waspCliNew,
      cdIntoCurrentProject,
      setDbToPSQL,
      appendToWaspFile jobDecl,
      createFile jobFile "./src/server/jobs" "bar.js",
      waspCliCompile
    ]

addGoogleAuth :: ShellCommandBuilder [ShellCommand]
addGoogleAuth = do
  let envFileContents =
        unlines
          [ "GOOGLE_CLIENT_ID=google_client_id",
            "GOOGLE_CLIENT_SECRET=google_client_secret"
          ]

  let authField =
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

  let userEntity =
        unlines
          [ "entity User {=psl",
            "  id                        Int           @id @default(autoincrement())",
            "  username                  String        @unique",
            "  password                  String",
            "  externalAuthAssociations  SocialLogin[]",
            "psl=}"
          ]

  let socialLoginEntity =
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

  sequence
    [ createFile envFileContents "./" ".env.server",
      insertLineAtWaspFileAfterVersion authField,
      appendToWaspFile userEntity,
      appendToWaspFile socialLoginEntity
    ]

addAction :: ShellCommandBuilder [ShellCommand]
addAction = do
  let actionDecl =
        unlines
          [ "action MySpecialAction {",
            "  handler: {",
            "    fn: import { foo } from \"@server/actions/bar.js\",",
            "    entities: [User],",
            "  }",
            "}"
          ]

  let actionFile =
        unlines
          [ "export const foo = async (args) => {",
            "  return 1",
            "}"
          ]

  sequence
    [ appendToWaspFile actionDecl,
      createFile actionFile "./src/server/actions" "bar.js"
    ]

addQuery :: ShellCommandBuilder [ShellCommand]
addQuery = do
  let queryDecl =
        unlines
          [ "query MySpecialQuery {",
            "  handler: {",
            "    fn: import { foo } from \"@server/queries/bar.js\",",
            "    entities: [User],",
            "  }",
            "}"
          ]

  let queryFile =
        unlines
          [ "export const foo = async (args) => {",
            "  return 1",
            "}"
          ]

  sequence
    [ appendToWaspFile queryDecl,
      createFile queryFile "./src/server/queries" "bar.js"
    ]

addDependencies :: ShellCommandBuilder [ShellCommand]
addDependencies = do
  let deps =
        unlines
          [ "  dependencies: [",
            "    (\"redux\", \"^4.0.5\"),",
            "    (\"react-redux\", \"^7.1.3\")",
            "  ],"
          ]

  sequence
    [ insertLineAtWaspFileAfterVersion deps
    ]

insertLineAtWaspFileAfterVersion :: String -> ShellCommandBuilder ShellCommand
insertLineAtWaspFileAfterVersion = flip insertLineAtWaspFile lineNumberInWaspFileAfterVersion
  where
    lineNumberInWaspFileAfterVersion :: Int
    lineNumberInWaspFileAfterVersion = 5
