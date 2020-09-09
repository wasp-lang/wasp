module Generator.DbGenerator
    ( genDb
    ) where

import qualified Path as P
import Data.Aeson ((.=), object)

import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import Wasp.EntityPSL (EntityPSL)
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

genDb :: Wasp -> CompileOptions -> [FileDraft]
genDb wasp _ =
    [ genPrismaSchema $ Wasp.getPSLEntities wasp
    ]

genPrismaSchema :: [EntityPSL] -> FileDraft
genPrismaSchema entities = createTemplateFileDraft dstPath tmplSrcPath (Just templateData)
    where
        relSrcPath :: Path (Rel DbTemplatesDir) File
        relSrcPath = (SP.fromPathRelFile [P.relfile|schema.prisma|])

        relDstPath :: Path (Rel DbRootDir) File
        -- Generated schema file will be in the same relative location as the
        -- template file within templates dir.
        relDstPath = SP.castRel relSrcPath

        dstPath = dbRootDirInProjectRootDir </> relDstPath
        tmplSrcPath = dbTemplatesDirInTemplatesDir </> relSrcPath

        templateData = object
            [ "entities" .= entities
            ]
