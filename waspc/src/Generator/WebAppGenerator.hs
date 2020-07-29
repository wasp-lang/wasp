module Generator.WebAppGenerator
       ( generateWebApp
       ) where

import Data.Aeson (ToJSON(..), (.=), object)
import qualified Path as P

import StrongPath (Path, Rel, Dir, (</>))
import qualified StrongPath as SP
import qualified Util
import CompileOptions (CompileOptions)
import Wasp
import Generator.FileDraft
import Generator.ExternalCodeGenerator (generateExternalCodeDir)
import qualified Generator.WebAppGenerator.EntityGenerator as EntityGenerator
import qualified Generator.WebAppGenerator.PageGenerator as PageGenerator
import qualified Generator.WebAppGenerator.ButtonGenerator as ButtonGenerator
import Generator.WebAppGenerator.Common (asTmplFile, asWebAppFile, asWebAppSrcFile)
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
generateReadme wasp = C.makeSimpleTemplateFD (asTmplFile [P.relfile|README.md|]) wasp

generatePackageJson :: Wasp -> FileDraft
generatePackageJson wasp = C.makeSimpleTemplateFD (asTmplFile [P.relfile|package.json|]) wasp

generateGitignore :: Wasp -> FileDraft
generateGitignore wasp = C.makeTemplateFD (asTmplFile [P.relfile|gitignore|])
                                          (asWebAppFile [P.relfile|.gitignore|])
                                          (Just $ toJSON wasp)

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp =
    C.copyTmplAsIs (asTmplFile [P.relfile|public/favicon.ico|])
    : map (\path -> C.makeSimpleTemplateFD (asTmplFile $ [P.reldir|public|] P.</> path) wasp)
        [ [P.relfile|index.html|]
        , [P.relfile|manifest.json|]
        ]

-- * Src dir

srcDir :: Path (Rel C.WebAppRootDir) (Dir C.WebAppSrcDir)
srcDir = C.webAppSrcDirInWebAppRootDir

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp
    = generateLogo
      : map makeSimpleSrcTemplateFD
        [ [P.relfile|index.js|]
        , [P.relfile|index.css|]
        , [P.relfile|router.js|]
        , [P.relfile|serviceWorker.js|]
        , [P.relfile|store/index.js|]
        , [P.relfile|store/middleware/logger.js|]
        ]
    ++ PageGenerator.generatePages wasp
    ++ EntityGenerator.generateEntities wasp
    ++ ButtonGenerator.generateButtons wasp
    ++ [generateReducersJs wasp]
  where
    generateLogo = C.makeTemplateFD (asTmplFile [P.relfile|src/logo.png|])
                                    (srcDir </> (asWebAppSrcFile [P.relfile|logo.png|]))
                                    Nothing
    makeSimpleSrcTemplateFD path = C.makeTemplateFD (asTmplFile $ [P.reldir|src|] P.</> path)
                                                    (srcDir </> (asWebAppSrcFile path))
                                                    (Just $ toJSON wasp)

generateReducersJs :: Wasp -> FileDraft
generateReducersJs wasp = C.makeTemplateFD tmplPath dstPath (Just templateData)
  where
    tmplPath = asTmplFile [P.relfile|src/reducers.js|]
    dstPath = srcDir </> (asWebAppSrcFile [P.relfile|reducers.js|])
    templateData = object
        [ "wasp" .= wasp
        , "entities" .= map toEntityData (getEntities wasp)
        ]
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityStatePath" .= ("./" ++ (SP.toFilePath $ EntityGenerator.entityStatePathInSrc entity))
        ]
