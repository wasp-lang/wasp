module Generator.Generators
       ( generateWebApp
       ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Path ((</>), reldir, relfile)
import qualified Path
import qualified Path.Aliases as Path

import CompileOptions (CompileOptions)
import qualified Util
import Wasp
import Generator.FileDraft
import qualified Generator.Entity as EntityGenerator
import qualified Generator.PageGenerator as PageGenerator
import qualified Generator.ExternalCode as ExternalCodeGenerator
import qualified Generator.Button
import qualified Generator.Common as Common


generateWebApp :: Wasp -> CompileOptions -> [FileDraft]
generateWebApp wasp options = concatMap ($ wasp)
    [ generateReadme
    , generatePackageJson
    , generateGitignore
    , generatePublicDir
    , generateSrcDir
    , ExternalCodeGenerator.generateExternalCodeDir options
    ]

generateReadme :: Wasp -> [FileDraft]
generateReadme wasp = [simpleTemplateFileDraft [relfile|README.md|] wasp]

generatePackageJson :: Wasp -> [FileDraft]
generatePackageJson wasp = [simpleTemplateFileDraft [relfile|package.json|] wasp]

generateGitignore :: Wasp -> [FileDraft]
generateGitignore wasp = [createTemplateFileDraft [relfile|.gitignore|] [relfile|gitignore|] (Just $ toJSON wasp)]

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp =
    createTemplateFileDraft ([reldir|public|] </> [relfile|favicon.ico|])
                            ([reldir|public|] </> [relfile|favicon.ico|])
                            Nothing
    : map (\path -> simpleTemplateFileDraft ([reldir|public|] </> path) wasp)
        [ [relfile|index.html|]
        , [relfile|manifest.json|]
        ]

-- * Src dir

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp
    = (createTemplateFileDraft (Common.srcDirPath </> [relfile|logo.png|])
                               ([reldir|src|] </> [relfile|logo.png|])
                               Nothing)
    : map (\path -> simpleTemplateFileDraft ([reldir|src|] </> path) wasp)
        [ [relfile|index.js|]
        , [relfile|index.css|]
        , [relfile|router.js|]
        , [relfile|serviceWorker.js|]
        , [reldir|store|] </> [relfile|index.js|]
        , [reldir|store|] </> [reldir|middleware|] </> [relfile|logger.js|]
        ]
    ++ PageGenerator.generatePages wasp
    ++ EntityGenerator.generateEntities wasp
    ++ Generator.Button.generateButtons wasp
    ++ [generateReducersJs wasp]

generateReducersJs :: Wasp -> FileDraft
generateReducersJs wasp = createTemplateFileDraft dstPath srcPath (Just templateData)
  where
    srcPath = [reldir|src|] </> [relfile|reducers.js|]
    dstPath = Common.srcDirPath </> [relfile|reducers.js|]
    templateData = object
        [ "wasp" .= wasp
        , "entities" .= map toEntityData (getEntities wasp)
        ]
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityStatePath" .= ("./" ++ (Path.toFilePath $ EntityGenerator.entityStatePathInSrc entity))
        ]

-- * Helpers

-- | Creates template file draft that uses given path as both src and dst path
--   and wasp as template data.
simpleTemplateFileDraft :: Path.RelFile -> Wasp -> FileDraft
simpleTemplateFileDraft path wasp = createTemplateFileDraft path path (Just $ toJSON wasp)
