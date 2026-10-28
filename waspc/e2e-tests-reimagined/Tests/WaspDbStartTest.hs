module Tests.WaspDbStartTest (waspDbStartTests) where

import Control.Monad (void)
import qualified Data.Char as Char
import qualified Data.Text as T
import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Db.Postgres (defaultPostgresPort)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres

waspDbStartTests :: [E2eTest]
waspDbStartTests =
  testGroup
    "wasp-db-start"
    [ test "fail-outside-project" $ waspFails ["db", "start"],
      test "succeed-sqlite-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["db", "start"],
      usingResources [devDatabasePort] $
        test "fail-postgresql-project-when-dev-db-not-running" $
          withNewWaspProject minimalStarterTemplate $ do
            setDbToPostgres
            waspFails ["db", "migrate-dev"] >>= assertOutputContains "The database needs to be running",
      usingResources [devDatabasePort] . requiringDocker $
        test "succeed-postgresql-project" $
          withNewWaspProject minimalStarterTemplate $ do
            setDbToPostgres

            -- Does `wasp db start` work?
            devDb <- startDevDb
            -- Does a Wasp command find the database once `wasp db start` says it's ready?
            waspDbMigrateDev "first_migration"
            -- Does a second `wasp db start` report the already running database?
            waspFails ["db", "start"] >>= assertOutputContains "already running on port"

            -- Does SIGINT stop the database and delete its container? If it
            -- didn't delete the container, the next `wasp db start` would fail
            -- with a container name conflict.
            volumeName <- getReportedVolumeName devDb
            stopDevDbWithSigint devDb volumeName

            -- Does `wasp db start` find another port when the default one is taken?
            occupyDefaultPostgresPort
            devDbOnOtherPort <- startDevDb
            -- Does a Wasp command find the database on a non-default port?
            waspDbMigrateDev "no_new_migration"
            freeDefaultPostgresPort
            stopDevDbWithSigint devDbOnOtherPort volumeName

            -- Can the user remove the volume reported by `wasp db start`?
            run $ command "docker" ["volume", "rm", volumeName]
    ]
  where
    startDevDb :: TestM BackgroundProcess
    startDevDb = do
      devDb <- startInBackground $ waspCli ["db", "start"]
      waitForOutput devDb "listening on IPv4 address" 180
      return devDb

    -- Stops the dev db the way Ctrl+C on `wasp db start` would.
    --
    -- We send SIGINT to the container instead of to `wasp db start`, because
    -- Wasp ignores SIGINT while it has a child process: it expects the child to
    -- handle the signal, and exits once the child exits. In a terminal, Ctrl+C
    -- signals the whole process group, so both of them get it.
    stopDevDbWithSigint :: BackgroundProcess -> String -> TestM ()
    stopDevDbWithSigint devDb volumeName = do
      containerIds <- lines . T.unpack . (.stdout) <$> run (command "docker" ["ps", "-q", "--filter", "volume=" ++ volumeName])
      mapM_ (\containerId -> run $ command "docker" ["kill", "--signal", "INT", containerId]) containerIds
      void $ waitForExit devDb

    -- The volume name as reported by `wasp db start`, since that's where users learn it from.
    getReportedVolumeName :: BackgroundProcess -> TestM String
    getReportedVolumeName devDb = do
      output <- getBackgroundOutput devDb
      let volumePrefix = T.pack Dev.Postgres.waspDevDbDockerVolumePrefix
      case filter (volumePrefix `T.isPrefixOf`) (T.words output) of
        word : _ -> return . T.unpack $ T.dropWhileEnd (not . isVolumeNameChar) word
        [] -> failTest "`wasp db start` didn't report its volume name."
      where
        isVolumeNameChar char = Char.isAlphaNum char || char `elem` ("-_." :: String)

    occupyDefaultPostgresPort :: TestM ()
    occupyDefaultPostgresPort = do
      let port = show defaultPostgresPort
      run $ command "docker" ["run", "-d", "--rm", "--name", portHolderContainerName, "-p", port ++ ":" ++ port, "alpine", "sleep", "300"]
      registerCleanupStep freeDefaultPostgresPort

    freeDefaultPostgresPort :: TestM ()
    freeDefaultPostgresPort = void $ runAllowingFailure (command "docker" ["rm", "-f", portHolderContainerName])

    portHolderContainerName = "wasp-e2e-tests-db-port-holder"
