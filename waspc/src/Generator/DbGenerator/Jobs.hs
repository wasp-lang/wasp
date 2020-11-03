module Generator.DbGenerator.Jobs
    ( migrateSave
    , migrateUp
    , generateClient
    , runStudio
    ) where

import           Generator.Common                 (ProjectRootDir)
import qualified Generator.Job                    as J
import           Generator.Job.Process            (runNodeCommandAsJob)
import           StrongPath                       (Abs, Dir, Path, (</>))
import qualified StrongPath                       as SP
import           Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)
import           Generator.DbGenerator            (dbSchemaFileInProjectRootDir)

-- | Runs `prisma migrate save` - creates migration folder for the latest schema changes.
migrateSave :: Path Abs (Dir ProjectRootDir) -> String -> J.Job
migrateSave projectDir migrationName = do
    let serverDir = projectDir </> serverRootDirInProjectRootDir
    let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

    -- NOTE(matija): We are running this command from server's root dir since that is where
    -- Prisma packages (cli and client) are currently installed.
    runNodeCommandAsJob serverDir "npx"
        [ "prisma", "migrate", "save"
        , "--schema", (SP.toFilePath schemaFile)
        , "--name", migrationName
        , "--create-db" -- Creates db if it doesn't already exist. Otherwise would stop and ask.
        , "--experimental"
        ] J.Db

-- | Runs `prisma migrate up` - applies all the available migrations. 
migrateUp :: Path Abs (Dir ProjectRootDir) -> J.Job
migrateUp projectDir = do
    let serverDir = projectDir </> serverRootDirInProjectRootDir
    let schemaFile = projectDir </> dbSchemaFileInProjectRootDir
    
    runNodeCommandAsJob serverDir "npx"
        [ "prisma", "migrate", "up"
        , "--schema", (SP.toFilePath schemaFile)
        , "--create-db" -- Creates db if it doesn't already exist. Otherwise would stop and ask.
        , "--experimental"
        ] J.Db

-- | Runs `prisma generate` - (re)generates db client api.
generateClient :: Path Abs (Dir ProjectRootDir) -> J.Job
generateClient projectDir = do
    let serverDir = projectDir </> serverRootDirInProjectRootDir
    let schemaFile = projectDir </> dbSchemaFileInProjectRootDir
    
    runNodeCommandAsJob serverDir "npx"
        [ "prisma", "generate"
        , "--schema", (SP.toFilePath schemaFile)
        ] J.Db

-- | Runs `prisma studio` - Prisma's db inspector.
runStudio :: Path Abs (Dir ProjectRootDir) -> J.Job
runStudio projectDir = do
    let serverDir = projectDir </> serverRootDirInProjectRootDir
    let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

    runNodeCommandAsJob serverDir "npx"
        [ "prisma", "studio"
        , "--schema", (SP.toFilePath schemaFile)
        , "--experimental"
        ] J.Db

