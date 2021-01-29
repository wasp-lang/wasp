module Generator.DbGenerator
    ( genDb
    , dbRootDirInProjectRootDir
    , dbSchemaFileInProjectRootDir
    ) where

import qualified Path as P
import Data.Aeson ((.=), object)

import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import CompileOptions (CompileOptions)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Generator.Common (ProjectRootDir)
import Generator.Templates (TemplatesDir)

-- * Path definitions

data DbRootDir
data DbTemplatesDir

dbRootDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir DbRootDir)
dbRootDirInProjectRootDir = SP.fromPathRelDir [P.reldir|db|]

dbTemplatesDirInTemplatesDir :: Path (Rel TemplatesDir) (Dir DbTemplatesDir)
dbTemplatesDirInTemplatesDir = SP.fromPathRelDir [P.reldir|db|]

dbSchemaFileInDbTemplatesDir :: Path (Rel DbTemplatesDir) File
dbSchemaFileInDbTemplatesDir = SP.fromPathRelFile [P.relfile|schema.prisma|]

dbSchemaFileInDbRootDir :: Path (Rel DbRootDir) File
-- Generated schema file will be in the same relative location as the
-- template file within templates dir.
dbSchemaFileInDbRootDir = SP.castRel dbSchemaFileInDbTemplatesDir

dbSchemaFileInProjectRootDir :: Path (Rel ProjectRootDir) File
dbSchemaFileInProjectRootDir = dbRootDirInProjectRootDir </> dbSchemaFileInDbRootDir

-- * Db generator

genDb :: Wasp -> CompileOptions -> [FileDraft]
genDb wasp _ =
    [ genPrismaSchema wasp
    ]

genPrismaSchema :: Wasp -> FileDraft
genPrismaSchema wasp = createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
    where
        dstPath = dbSchemaFileInProjectRootDir
        tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir

        isBuild = Wasp.getIsBuild wasp

        datasourceProvider = if isBuild then "postgresql" else "sqlite"
        datasourceUrl = if isBuild then "env(\"DATABASE_URL\")" else "\"file:./dev.db\""

        templateData = object
            [ "entities" .= (Wasp.getPSLEntities wasp)
            , "datasourceProvider" .= (datasourceProvider :: String)
            , "datasourceUrl"      .= (datasourceUrl :: String)
            ]
