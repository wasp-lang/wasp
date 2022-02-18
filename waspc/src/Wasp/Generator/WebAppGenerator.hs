module Wasp.Generator.WebAppGenerator
  ( generateWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import StrongPath
  ( Dir,
    Path',
    Rel,
    reldir,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getApp)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.Generator.ExternalCodeGenerator (generateExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.WebAppGenerator.AuthG as AuthG
import Wasp.Generator.WebAppGenerator.Common
  ( asTmplFile,
    asWebAppFile,
    asWebAppSrcFile,
  )
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.ExternalCodeGenerator as WebAppExternalCodeGenerator
import Wasp.Generator.WebAppGenerator.OperationsGenerator (genOperations)
import qualified Wasp.Generator.WebAppGenerator.RouterGenerator as RouterGenerator
import Wasp.Util ((<++>))

generateWebApp :: AppSpec -> Generator [FileDraft]
generateWebApp spec = do
  sequence
    [ generateReadme,
      genPackageJson spec npmDepsForWasp,
      generateGitignore,
      return $ C.mkTmplFd $ asTmplFile [relfile|netlify.toml|]
    ]
    <++> generatePublicDir spec
    <++> generateSrcDir spec
    <++> generateExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy (AS.externalCodeFiles spec)

generateReadme :: Generator FileDraft
generateReadme = return $ C.mkTmplFd $ asTmplFile [relfile|README.md|]

genPackageJson :: AppSpec -> N.NpmDepsForWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  combinedDependencies <- N.genNpmDepsForPackage spec waspDependencies
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asWebAppFile [relfile|package.json|])
      ( Just $
          object
            [ "appName" .= (fst (getApp spec) :: String),
              "depsChunk" .= N.getDependenciesPackageJsonEntry combinedDependencies,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry combinedDependencies
            ]
      )

npmDepsForWasp :: N.NpmDepsForWasp
npmDepsForWasp =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("axios", "^0.21.1"),
            ("lodash", "^4.17.15"),
            ("react", "^16.12.0"),
            ("react-dom", "^16.12.0"),
            ("react-query", "^2.14.1"),
            ("react-router-dom", "^5.1.2"),
            ("react-scripts", "4.0.3"),
            ("uuid", "^3.4.0")
          ],
      N.waspDevDependencies =
        AS.Dependency.fromList
          []
    }

generateGitignore :: Generator FileDraft
generateGitignore =
  return $
    C.mkTmplFdWithDst
      (asTmplFile [relfile|gitignore|])
      (asWebAppFile [relfile|.gitignore|])

generatePublicDir :: AppSpec -> Generator [FileDraft]
generatePublicDir spec = do
  publicIndexHtmlFd <- generatePublicIndexHtml spec
  return $
    C.mkTmplFd (asTmplFile [relfile|public/favicon.ico|]) :
    publicIndexHtmlFd :
    ( let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          processPublicTmpl path = C.mkTmplFdWithData (asTmplFile $ [reldir|public|] </> path) tmplData
       in processPublicTmpl
            <$> [ [relfile|manifest.json|]
                ]
    )

generatePublicIndexHtml :: AppSpec -> Generator FileDraft
generatePublicIndexHtml spec =
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile [relfile|public/index.html|])
      targetPath
      (Just templateData)
  where
    targetPath = [relfile|public/index.html|]
    templateData =
      object
        [ "title" .= (AS.App.title (snd $ getApp spec) :: String),
          "head" .= (maybe "" (intercalate "\n") (AS.App.head $ snd $ getApp spec) :: String)
        ]

-- * Src dir

srcDir :: Path' (Rel C.WebAppRootDir) (Dir C.WebAppSrcDir)
srcDir = C.webAppSrcDirInWebAppRootDir

-- TODO(matija): Currently we also generate auth-specific parts in this file (e.g. token management),
-- although they are not used anywhere outside.
-- We could further "templatize" this file so only what is needed is generated.
--

-- | Generates api.js file which contains token management and configured api (e.g. axios) instance.
genApi :: FileDraft
genApi = C.mkTmplFd (C.asTmplFile [relfile|src/api.js|])

generateSrcDir :: AppSpec -> Generator [FileDraft]
generateSrcDir spec = do
  routerFd <- RouterGenerator.generateRouter spec
  operationsFds <- genOperations spec
  authFds <- AuthG.genAuth spec

  return $
    generateLogo :
    routerFd :
    genApi :
    map
      processSrcTmpl
      [ [relfile|index.js|],
        [relfile|index.css|],
        [relfile|serviceWorker.js|],
        [relfile|config.js|],
        [relfile|queryCache.js|],
        [relfile|utils.js|]
      ]
      ++ operationsFds
      ++ authFds
  where
    generateLogo =
      C.mkTmplFdWithDstAndData
        (asTmplFile [relfile|src/logo.png|])
        (srcDir </> asWebAppSrcFile [relfile|logo.png|])
        Nothing
    processSrcTmpl path =
      C.mkTmplFdWithDst
        (asTmplFile $ [reldir|src|] </> path)
        (srcDir </> asWebAppSrcFile path)
