module Generator.WebAppGenerator
       ( generateWebApp
       ) where

import Data.Aeson (ToJSON(..), (.=), object)
import qualified Path
import Path ((</>), reldir, relfile)

import qualified Util
import qualified Path.Aliases as Path
import CompileOptions (CompileOptions)
import Wasp
import Generator.FileDraft
import Generator.ExternalCodeGenerator (generateExternalCodeDir)
import qualified Generator.WebAppGenerator.EntityGenerator as EntityGenerator
import qualified Generator.WebAppGenerator.PageGenerator as PageGenerator
import qualified Generator.WebAppGenerator.ButtonGenerator as ButtonGenerator
import qualified Generator.WebAppGenerator.Common as C
import qualified Generator.WebAppGenerator.ExternalCodeGenerator as WebAppExternalCodeGenerator


generateWebApp :: Wasp -> CompileOptions -> [FileDraft]
generateWebApp wasp _ = concatMap ($ wasp)
    [ (:[]) . generateReadme
    , (:[]) . generatePackageJson
    , (:[]) . generateGitignore
    , generatePublicDir
    , generateSrcDir
    , generateExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy
    ]

generateReadme :: Wasp -> FileDraft
generateReadme wasp = C.makeSimpleTemplateFD [relfile|README.md|] wasp

generatePackageJson :: Wasp -> FileDraft
generatePackageJson wasp = C.makeSimpleTemplateFD [relfile|package.json|] wasp

generateGitignore :: Wasp -> FileDraft
generateGitignore wasp = C.makeTemplateFD [relfile|gitignore|] [relfile|.gitignore|] (Just $ toJSON wasp)

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp =
    C.copyTmplAsIs [relfile|public/favicon.ico|]
    : map (\path -> C.makeSimpleTemplateFD ([reldir|public|] </> path) wasp)
        [ [relfile|index.html|]
        , [relfile|manifest.json|]
        ]

-- * Src dir

srcDir :: Path.RelDir
srcDir = C.webAppSrcDirInWebAppRootDir

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp
    = (C.makeTemplateFD [relfile|src/logo.png|] (srcDir </> [relfile|logo.png|]) Nothing)
    : map (\path -> C.makeTemplateFD ([reldir|src|] </> path) (srcDir </> path) (Just $ toJSON wasp))
        [ [relfile|index.js|]
        , [relfile|index.css|]
        , [relfile|router.js|]
        , [relfile|serviceWorker.js|]
        , [relfile|store/index.js|]
        , [relfile|store/middleware/logger.js|]
        ]
    ++ PageGenerator.generatePages wasp
    ++ EntityGenerator.generateEntities wasp
    ++ ButtonGenerator.generateButtons wasp
    ++ [generateReducersJs wasp]

generateReducersJs :: Wasp -> FileDraft
generateReducersJs wasp = C.makeTemplateFD srcPath dstPath (Just templateData)
  where
    srcPath = [relfile|src/reducers.js|]
    dstPath = srcDir </> [relfile|reducers.js|]
    templateData = object
        [ "wasp" .= wasp
        , "entities" .= map toEntityData (getEntities wasp)
        ]
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityStatePath" .= ("./" ++ (Path.toFilePath $ EntityGenerator.entityStatePathInSrc entity))
        ]
