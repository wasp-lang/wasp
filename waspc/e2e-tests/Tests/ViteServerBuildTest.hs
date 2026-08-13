module Tests.ViteServerBuildTest (viteServerBuildTest) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    appendToFile,
    appendToPrismaFile,
    createSeedFile,
    createTestWaspProject,
    inTestWaspProjectDir,
    replaceMainWaspTsFile,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliCompile,
    writeToFile,
    (~&&),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.ServerGenerator.Common
  ( ServerBundleDir,
    ServerRootDir,
    serverBundleDirInServerRootDir,
    serverRootDirInGeneratedAppDir,
  )
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Project.Env (dotEnvServer)
import Wasp.Version (waspVersion)

-- | Tests bundling the server, which is a Vite build of the `server`
-- environment declared by the `waspServer()` plugin.
viteServerBuildTest :: Test
viteServerBuildTest =
  Test
    "vite-server-build"
    [ TestCase
        "succeed-bundling-server"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  bundleServer,
                  assertBundleFileExists "server.js",
                  expectCommandFailure <$> assertBundleFileExists "dbSeed.js"
                ]
            ]
        ),
      TestCase
        "bundle-db-seed-when-seeds-are-defined"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                defineSeed
                  ++ [ setWaspDbToPSQL,
                       waspCliBuild,
                       bundleServer,
                       assertBundleFileExists "server.js",
                       assertBundleFileExists "dbSeed.js"
                     ]
            ]
        ),
      TestCase
        "fail-on-server-code-type-error"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                defineSeed
                  ++ [ setWaspDbToPSQL,
                       waspCliBuild,
                       addTypeErrorToSeedFile,
                       expectCommandFailure <$> bundleServer
                     ]
            ]
        ),
      TestCase
        -- The server reads its env vars at runtime, so none of their values
        -- may end up inlined in the bundle.
        "ignore-dotenv-server-file-in-bundle"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ writeDotEnvServerFile,
                  waspCliCompile,
                  bundleServer,
                  assertBundleFileExists "server.js",
                  expectCommandFailure <$> assertBundleContains dotEnvServerFileValue
                ]
            ]
        )
    ]

-- | Runs the bundling in a subshell, so that the command stays a single unit
-- (e.g. negatable with 'expectCommandFailure') and doesn't change the working
-- directory of the commands that follow it.
bundleServer :: ShellCommandBuilder WaspProjectContext ShellCommand
bundleServer = do
  context <- ask
  let bundleInServerDir = ("cd " ++ fromAbsDir (serverDir context)) ~&& "npm run bundle"
  return $ "(" ++ bundleInServerDir ++ ")"

assertBundleFileExists :: FilePath -> ShellCommandBuilder WaspProjectContext ShellCommand
assertBundleFileExists fileName = do
  context <- ask
  return $ "test -f " ++ fromAbsDir (bundleDir context) ++ fileName

assertBundleContains :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
assertBundleContains value = do
  context <- ask
  return $ "grep -r '" ++ value ++ "' " ++ fromAbsDir (bundleDir context)

-- | Defines a seed function, which makes Wasp generate a second bundle entry.
defineSeed :: [ShellCommandBuilder WaspProjectContext ShellCommand]
defineSeed =
  [ appendToPrismaFile taskPrismaModel,
    createSeedFile (T.unpack seedName ++ ".ts") seedFileContent,
    replaceMainWaspTsFile mainWaspTsWithSeeds
  ]

addTypeErrorToSeedFile :: ShellCommandBuilder WaspProjectContext ShellCommand
addTypeErrorToSeedFile =
  appendToFile ("src/db/" ++ T.unpack seedName ++ ".ts") "const shouldBeString: string = 123"

writeDotEnvServerFile :: ShellCommandBuilder WaspProjectContext ShellCommand
writeDotEnvServerFile = do
  context <- ask
  writeToFile (context.waspProjectDir </> dotEnvServer) $
    T.pack $
      "MY_SERVER_SECRET=" ++ dotEnvServerFileValue

serverDir :: WaspProjectContext -> Path' Abs (Dir ServerRootDir)
serverDir context =
  context.waspProjectDir
    </> generatedAppDirInWaspProjectDir
    </> serverRootDirInGeneratedAppDir

bundleDir :: WaspProjectContext -> Path' Abs (Dir ServerBundleDir)
bundleDir context = serverDir context </> serverBundleDirInServerRootDir

dotEnvServerFileValue :: String
dotEnvServerFileValue = "DotEnvServerFileValue"

seedName :: T.Text
seedName = "seedTasks"

seedFileContent :: T.Text
seedFileContent =
  [trimming|
    import { prisma } from 'wasp/server'

    export async function $seedName() {
      await prisma.task.create({
        data: { description: 'Test task', isDone: false }
      })
    }
  |]

taskPrismaModel :: T.Text
taskPrismaModel =
  [trimming|
    model Task {
      id          Int     @id @default(autoincrement())
      description String
      isDone      Boolean @default(false)
    }
  |]

mainWaspTsWithSeeds :: T.Text
mainWaspTsWithSeeds =
  [trimming|
    import { app, page, route } from "@wasp.sh/spec";
    import { MainPage } from "./src/MainPage" with { type: "ref" };
    import { $seedName } from "./src/db/$seedName" with { type: "ref" };

    export default app({
      name: "viteServerBuildTest",
      title: "viteServerBuildTest",
      wasp: { version: "$textWaspVersion" },
      head: ["<link rel='icon' href='/favicon.ico' />"],
      db: {
        seeds: [$seedName]
      },
      spec: [
        route("RootRoute", "/", page(MainPage)),
      ]
    })
  |]
  where
    textWaspVersion = T.pack . show $ waspVersion

expectCommandFailure :: ShellCommand -> ShellCommand
expectCommandFailure command = "! " ++ command
