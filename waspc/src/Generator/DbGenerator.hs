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
import Wasp.Entity (Entity)
import CompileOptions (CompileOptions)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Generator.Common (ProjectRootDir)
import Generator.Templates (TemplatesDir)

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

genDb :: Wasp -> CompileOptions -> [FileDraft]
genDb wasp _ =
    [ genPrismaSchema $ Wasp.getPSLEntities wasp
    ]

genPrismaSchema :: [Entity] -> FileDraft
genPrismaSchema entities = createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
    where
        dstPath = dbSchemaFileInProjectRootDir
        tmplSrcPath = dbTemplatesDirInTemplatesDir </> dbSchemaFileInDbTemplatesDir

        templateData = object
            [ "entities" .= entities
            ]
