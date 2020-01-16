module Generator.PageGenerator
       ( generatePages

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       , generatePageComponent
       , generatePageStyle
       ) where

import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import System.FilePath (FilePath, (</>), (<.>))
import qualified System.FilePath as FilePath

import qualified Util
import Wasp
import Generator.FileDraft
import qualified Generator.EntityGenerator as EntityGenerator
import Generator.ExternalCode (externalCodeDirPathInSrc)
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
    srcPath = "src" </> "_Page.js"
    dstPath = FilePath.normalise $ Common.srcDirPath </> pageDirPathInSrc </> (pageName page) <.> "js"
    templateData = object $
        [ "wasp" .= wasp
        , "page" .= page
        , "entities" .= map toEntityData (getEntities wasp)
        , "jsImports" .= map toJsImportData (getJsImports wasp)
        ]
        ++ maybe []
                 (\_ -> ["pageStylePath" .= (relPathFromPageToSrc </> pageStylePathInSrcDir page)])
                 (pageStyle page)

    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityUpperName" .= (Util.toUpperFirst $ entityName entity)
        , "entityStatePath" .= (relPathFromPageToSrc </> EntityGenerator.entityStatePathInSrc entity)
        , "entityActionsPath" .= (relPathFromPageToSrc </> EntityGenerator.entityActionsPathInSrc entity)
        , "entityClassPath" .= (relPathFromPageToSrc </> EntityGenerator.entityClassPathInSrc entity)
        , "entityCreateForms" .= map toEntityFormData entityForms

        -- TODO(matija): this will become "entityLists"
        , "entityListPath" .= (relPathFromPageToSrc </> EntityGenerator.entityListPathInSrc entity)
        ]
        where
            entityForms = getEntityFormsForEntity wasp entity
            generateEntityFormPath entityForm = relPathFromPageToSrc </>
                (EntityGenerator.entityCreateFormPathInSrc entity entityForm)

            toEntityFormData entityForm = object
                [ "entityForm" .= entityForm
                , "path" .= generateEntityFormPath entityForm
                ]

    toJsImportData :: JsImport -> Aeson.Value
    toJsImportData jsImport = object
        [ "what" .= jsImportWhat jsImport
        -- NOTE: Here we assume that "from" is relative to external code dir path.
        --   If this part will be reused, consider externalizing this assumption, so we don't have it on multiple places.
        , "from" .= (relPathFromPageToSrc </> externalCodeDirPathInSrc </> jsImportFrom jsImport)
        ]

-- | NOTE: If you modify this value, make sure to also accordingly update relPathFromPageToSrc.
--     For example, if pageDirPathInSrc = "foo/bar", then relPathFromPageToSrc should be "../../".
pageDirPathInSrc :: FilePath
pageDirPathInSrc = "."
-- | Relative path from page to the /src directory.
--   It is the opposite of pageDirPathInSrc and should be updated together with it.
--   TODO: We could deduce this directly from the pageDirPathInSrc instead of hardcoding it.
relPathFromPageToSrc :: FilePath
relPathFromPageToSrc = "."

generatePageStyle :: Wasp -> Page -> [FileDraft]
generatePageStyle _ page = maybe
    []
    (\style -> [createTextFileDraft dstPath style])
    (pageStyle page)
  where
    dstPath = Common.srcDirPath </> pageStylePathInSrcDir page

pageStylePathInSrcDir :: Page -> FilePath
pageStylePathInSrcDir page = (pageName page) <.> "css"
