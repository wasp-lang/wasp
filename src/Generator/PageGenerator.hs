module Generator.PageGenerator
       ( generatePages

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       , generatePageComponent
       , generatePageStyle
       ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified System.FilePath as FP
import Path ((</>), relfile, reldir)
import qualified Path
import qualified Path.Aliases as Path
import qualified Path.Extra as Path

import qualified Util
import Wasp
import Generator.FileDraft
import qualified Generator.Entity as EntityGenerator
import Generator.ExternalCode.Common (externalCodeDirPathInSrc)
import qualified Generator.Common as Common


generatePages :: Wasp -> [FileDraft]
generatePages wasp = concatMap (generatePage wasp) (getPages wasp)

generatePage :: Wasp -> Page -> [FileDraft]
generatePage wasp page =
    [ generatePageComponent wasp page
    ]
    ++ generatePageStyle wasp page

generatePageComponent :: Wasp -> Page -> FileDraft
generatePageComponent wasp page = createTemplateFileDraft dstPath srcPath (Just templateData)
  where
    srcPath = [reldir|src|] </> [relfile|_Page.js|]
    dstPath = Common.srcDirPath </> pageDirPathInSrc </> (fromJust $ Path.parseRelFile $ (pageName page) ++ ".js")
    templateData = object $
        [ "wasp" .= wasp
        , "page" .= page
        , "entities" .= map toEntityData (getEntities wasp)
        , "jsImports" .= map toJsImportData (getJsImports wasp)
        ]
        ++ maybe []
                 (\_ -> ["pageStylePath" .= (buildImportPathFromPathInSrc $  pageStylePathInSrcDir page)])
                 (pageStyle page)

    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityUpperName" .= (Util.toUpperFirst $ entityName entity)
        , "entityStatePath" .=
            (buildImportPathFromPathInSrc $ EntityGenerator.entityStatePathInSrc entity)
        , "entityActionsPath" .=
            (buildImportPathFromPathInSrc $ EntityGenerator.entityActionsPathInSrc entity)
        , "entityClassPath" .=
            (buildImportPathFromPathInSrc $ EntityGenerator.entityClassPathInSrc entity)
        , "entityCreateForms" .= map toEntityFormData entityForms

        -- TODO(matija): this will become "entityLists"
        , "entityListPath" .= (buildImportPathFromPathInSrc $ EntityGenerator.entityListPathInSrc entity)
        ]
        where
            entityForms = getEntityFormsForEntity wasp entity
            generateEntityFormPath entityForm =
                buildImportPathFromPathInSrc $ EntityGenerator.entityCreateFormPathInSrc entity entityForm

            toEntityFormData entityForm = object
                [ "entityForm" .= entityForm
                , "path" .= generateEntityFormPath entityForm
                ]

    toJsImportData :: Wasp.JsImport -> Aeson.Value
    toJsImportData jsImport = object
        [ "what" .= jsImportWhat jsImport
        -- NOTE: Here we assume that "from" is relative to external code dir path.
        --   If this part will be reused, consider externalizing this assumption, so we don't have it on multiple places.
        , "from" .= (buildImportPathFromPathInSrc $ externalCodeDirPathInSrc </>
                     (fromJust $ Path.parseRelFile $ jsImportFrom jsImport))
        ]

pageDirPathInSrc :: Path.RelDir
pageDirPathInSrc = [reldir|.|]

relPathFromPageToSrc :: FilePath
relPathFromPageToSrc = Path.reversePath pageDirPathInSrc

-- | Takes path relative to the src path of generated project and turns it into relative path that can be
-- used as "from" part of the import in the Page source file.
-- NOTE: Here we return FilePath instead of Path because we need stuff like "./" or "../" in the path,
-- which Path would normalize away.
buildImportPathFromPathInSrc :: Path.Path Path.Rel a -> FilePath
buildImportPathFromPathInSrc pathInSrc = "." FP.</> relPathFromPageToSrc FP.</> (Path.toFilePath pathInSrc)

generatePageStyle :: Wasp -> Page -> [FileDraft]
generatePageStyle _ page = maybe
    []
    (\style -> [createTextFileDraft dstPath style])
    (pageStyle page)
  where
    dstPath = Common.srcDirPath </> pageStylePathInSrcDir page

pageStylePathInSrcDir :: Page -> Path.RelFile
pageStylePathInSrcDir page = fromJust $ Path.parseRelFile $ (pageName page) ++ ".css"
