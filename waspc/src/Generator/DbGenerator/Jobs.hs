module Generator.DbGenerator.Jobs
    ( migrateSave
    ) where

import           Generator.Common                 (ProjectRootDir)
import qualified Generator.Job                    as J
import           Generator.Job.Process            (runNodeCommandAsJob)
import           StrongPath                       (Abs, Dir, Path, (</>))
import qualified StrongPath                       as SP
import           Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)
import           Generator.DbGenerator            (dbSchemaFileInProjectRootDir)

migrateSave :: Path Abs (Dir ProjectRootDir) -> String -> J.Job
migrateSave projectDir migrationName = do
    let serverDir = projectDir </> serverRootDirInProjectRootDir
    let schemaFile = projectDir </> dbSchemaFileInProjectRootDir

    -- TODO(matija): should we make the type here J.Db? Although actually we are running this
    -- from server currently so maybe it is ok? Or maybe that is internal stuff?
    --
    -- NOTE(matija): We are running this command from server's root dir since that is where
    -- Prisma packages (cli and client) are currently installed.
    runNodeCommandAsJob serverDir "npx"
        [ "prisma", "migrate", "save"
        , "--schema", (SP.toFilePath schemaFile)
        , "--name", migrationName
        , "--create-db" -- Creates db if it doesn't already exist. Otherwise would stop and ask.
        , "--experimental"
        ] J.Server
