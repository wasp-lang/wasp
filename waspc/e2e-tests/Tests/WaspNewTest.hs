module Tests.WaspNewTest (waspNewTest) where

import Command (Command, withStdin)
import qualified Data.Text as T
import SharedActions (runCommand, waspCli, waspCliNew)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (basicStarterTemplate, minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)

waspNewTest :: Test
waspNewTest =
  Test
    "wasp-new"
    [ TestCase "create-minimal" $
        runCommand $
          waspCliNew "wasp-app" minimalStarterTemplate,
      TestCase "create-minimal-interactive" $
        runCommand $
          waspCliNewInteractive "wasp-app" minimalStarterTemplate,
      TestCase "create-basic" $
        runCommand $
          waspCliNew "wasp-app" basicStarterTemplate,
      TestCase "create-basic-interactive" $
        runCommand $
          waspCliNewInteractive "wasp-app" basicStarterTemplate
          -- FIXME(Franjo): These will fail because there "dev" opensaas template tag doesn't exist.
          -- We can't really test the `saas` template in `waspc` e2e tests.
          -- TestCase "create-saas" $
          --   runCommand $ waspCliNew "wasp-app" SaaS,
          -- TestCase "create-saas-interactive" $
          --   runCommand $ waspCliNewInteractive "wasp-app" SaaS
    ]

-- | Runs `wasp new` without arguments, answering its prompts through stdin.
waspCliNewInteractive :: String -> StarterTemplate -> Command
waspCliNewInteractive appName starterTemplate =
  withStdin (T.pack $ appName ++ "\n" ++ show starterTemplate ++ "\n") (waspCli ["new"])
