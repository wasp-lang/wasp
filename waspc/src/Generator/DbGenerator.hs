module Generator.DbGenerator
    ( genDb
    , dbRootDirInProjectRootDir
    , dbSchemaFileInProjectRootDir
    ) where

import           Data.Aeson          (object, (.=))
import qualified Path                as P
import           Data.Maybe          (fromMaybe)

import           CompileOptions      (CompileOptions)
import           Generator.Common    (ProjectRootDir)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Generator.Templates (TemplatesDir)
import qualified Psl.Ast.Model
import qualified Psl.Generator.Model
import           StrongPath          (Dir, File, Path, Rel, (</>))
import qualified StrongPath          as SP
import           Wasp                (Wasp)
import qualified Wasp
import qualified Wasp.Db
import           Wasp.Entity         (Entity)
import qualified Wasp.Entity

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

        templateData = object
            [ "modelSchemas" .= map entityToPslModelSchema (Wasp.getPSLEntities wasp)
            , "datasourceProvider" .= (datasourceProvider :: String)
            , "datasourceUrl"      .= (datasourceUrl :: String)
            ]

        dbSystem = fromMaybe Wasp.Db.SQLite $ Wasp.Db._system <$> Wasp.getDb wasp
        (datasourceProvider, datasourceUrl) = case dbSystem of
            Wasp.Db.PostgreSQL -> ("postgresql", "env(\"DATABASE_URL\")")
                                  -- TODO: Report this error with some better mechanism, not `error`.
            Wasp.Db.SQLite     -> if Wasp.getIsBuild wasp
                                  then error "SQLite is not supported in production. Set db.system to smth else."
                                  else ("sqlite",     "\"file:./dev.db\"")

        entityToPslModelSchema :: Entity -> String
        entityToPslModelSchema entity = Psl.Generator.Model.generateModel $
            Psl.Ast.Model.Model (Wasp.Entity._name entity) (Wasp.Entity._pslModelBody entity)
