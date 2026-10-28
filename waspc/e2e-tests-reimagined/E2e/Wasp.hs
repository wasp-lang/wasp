-- | Steps for working with the Wasp CLI and Wasp projects.
module E2e.Wasp
  ( waspAppDirName,
    wasp,
    waspFails,
    createWaspProject,
    inWaspProject,
    withNewWaspProject,
    waspDbMigrateDev,
    assertMigrationExists,
    setDbToPostgres,
    appendToPrismaSchema,
    writeMainWaspTs,
    writeSeedFile,
    buildAndRemoveDockerImage,
    textWaspVersion,
  )
where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as T
import E2e.Files (appendToFile, replaceInFile, writeToFile)
import E2e.Process (ProcessResult, command, run, runExpectingFailure, waspCli)
import E2e.TestM (TestM, failTest, inDirectory, resolvePath, whenDockerEnabled)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, listDirectory, renameDirectory)
import System.FilePath ((</>))
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import Wasp.Version (waspVersion)

-- | The directory (and the name) of the Wasp project a test creates.
waspAppDirName :: FilePath
waspAppDirName = "wasp-app"

-- | Runs a Wasp CLI command and fails the test if it doesn't succeed.
wasp :: [String] -> TestM ProcessResult
wasp = run . waspCli

-- | Runs a Wasp CLI command and fails the test if it succeeds.
waspFails :: [String] -> TestM ProcessResult
waspFails = runExpectingFailure . waspCli

createWaspProject :: StarterTemplate -> TestM ()
createWaspProject template = void $ wasp ["new", waspAppDirName, "-t", show template]

inWaspProject :: TestM a -> TestM a
inWaspProject = inDirectory waspAppDirName

withNewWaspProject :: StarterTemplate -> TestM a -> TestM a
withNewWaspProject template steps = createWaspProject template >> inWaspProject steps

-- | Runs `wasp db migrate-dev` in the Wasp project.
--
-- We make the migration name deterministic by renaming its directory to
-- @no-date-<migrationName>@, instead of the usual @<date>_<migrationName>@.
-- Snapshots shouldn't change with every run.
-- Caveat: a project can't have two migrations with the same name.
waspDbMigrateDev :: String -> TestM ()
waspDbMigrateDev migrationName = do
  wasp ["db", "migrate-dev", "--name", migrationName]
  migrationsDirs >>= mapM_ removeDatePrefix
  where
    removeDatePrefix migrationsDir = liftIO $ do
      exists <- doesDirectoryExist migrationsDir
      -- When there is nothing to migrate, `wasp db migrate-dev` creates no migration.
      entries <- if exists then listDirectory migrationsDir else return []
      forM_ (filter (("_" ++ migrationName) `isSuffixOf`) entries) $ \migrationDir ->
        renameDirectory (migrationsDir </> migrationDir) (migrationsDir </> ("no-date-" ++ migrationName))

assertMigrationExists :: String -> TestM ()
assertMigrationExists migrationName =
  migrationsDirs >>= mapM_ assertContainsMigration
  where
    assertContainsMigration migrationsDir = do
      exists <- liftIO $ doesDirectoryExist migrationsDir
      entries <- if exists then liftIO (listDirectory migrationsDir) else return []
      if any (migrationName `isInfixOf`) entries
        then return ()
        else failTest $ "Expected a migration named " ++ show migrationName ++ " in " ++ migrationsDir

-- | The migrations dir in the project and its copy in the generated app.
migrationsDirs :: TestM [FilePath]
migrationsDirs =
  mapM
    resolvePath
    [ SP.fromRelDir dbMigrationsDirInWaspProjectDir,
      SP.fromRelDir $
        dotWaspDirInWaspProjectDir
          SP.</> generatedAppDirInDotWaspDir
          SP.</> dbRootDirInGeneratedAppDir
          SP.</> dbMigrationsDirInDbRootDir
    ]

setDbToPostgres :: TestM ()
setDbToPostgres = replaceInFile "schema.prisma" "provider = \"sqlite\"" "provider = \"postgresql\""

appendToPrismaSchema :: T.Text -> TestM ()
appendToPrismaSchema = appendToFile "schema.prisma"

writeMainWaspTs :: T.Text -> TestM ()
writeMainWaspTs = writeToFile "main.wasp.ts"

writeSeedFile :: FilePath -> T.Text -> TestM ()
writeSeedFile fileName = writeToFile ("src/db" </> fileName)

-- | Builds the Docker image of the generated app, to check that the Dockerfile works.
-- Skipped when Docker is disabled.
buildAndRemoveDockerImage :: TestM ()
buildAndRemoveDockerImage =
  whenDockerEnabled $ do
    let imageTag = "waspc-e2e-tests-" ++ waspAppDirName
    inDirectory (SP.fromRelDir $ dotWaspDirInWaspProjectDir SP.</> generatedAppDirInDotWaspDir) $ do
      run $ command "docker" ["build", "--build-arg", "BUILDKIT_DOCKERFILE_CHECK=error=true", "-t", imageTag, "."]
      void . run $ command "docker" ["image", "rm", imageTag]

-- | For use in @main.wasp.ts@ files.
textWaspVersion :: T.Text
textWaspVersion = T.pack $ show waspVersion
