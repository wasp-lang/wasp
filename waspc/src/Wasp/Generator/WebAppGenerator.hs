module Wasp.Generator.WebAppGenerator
  ( generateWebApp,
    waspNpmDeps,
  )
where

import Data.Aeson
  ( ToJSON (..),
    object,
    (.=),
  )
import Data.List (intercalate)
import StrongPath
  ( Dir,
    Path',
    Rel,
    reldir,
    relfile,
    (</>),
  )
import Wasp.CompileOptions (CompileOptions)
import Wasp.Generator.ExternalCodeGenerator (generateExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.PackageJsonGenerator
  ( npmDepsToPackageJsonEntry,
    resolveNpmDeps,
  )
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
import qualified Wasp.NpmDependency as ND
import Wasp.Wasp
import qualified Wasp.Wasp.App as Wasp.App
import qualified Wasp.Wasp.NpmDependencies as WND

generateWebApp :: Wasp -> CompileOptions -> [FileDraft]
generateWebApp wasp _ =
  concat
    [ [generateReadme wasp],
      [genPackageJson wasp waspNpmDeps],
      [generateGitignore wasp],
      generatePublicDir wasp,
      generateSrcDir wasp,
      generateExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy wasp,
      [C.makeSimpleTemplateFD (asTmplFile [relfile|netlify.toml|]) wasp]
    ]

generateReadme :: Wasp -> FileDraft
generateReadme wasp = C.makeSimpleTemplateFD (asTmplFile [relfile|README.md|]) wasp

genPackageJson :: Wasp -> [ND.NpmDependency] -> FileDraft
genPackageJson wasp waspDeps =
  C.makeTemplateFD
    (C.asTmplFile [relfile|package.json|])
    (C.asWebAppFile [relfile|package.json|])
    ( Just $
        object
          [ "wasp" .= wasp,
            "depsChunk" .= npmDepsToPackageJsonEntry (resolvedWaspDeps ++ resolvedUserDeps)
          ]
    )
  where
    (resolvedWaspDeps, resolvedUserDeps) =
      case resolveNpmDeps waspDeps userDeps of
        Right deps -> deps
        Left depsAndErrors -> error $ intercalate " ; " $ map snd depsAndErrors

    userDeps :: [ND.NpmDependency]
    userDeps = WND._dependencies $ Wasp.Wasp.getNpmDependencies wasp

waspNpmDeps :: [ND.NpmDependency]
waspNpmDeps =
  ND.fromList
    [ ("axios", "^0.21.1"),
      ("lodash", "^4.17.15"),
      ("react", "^16.12.0"),
      ("react-dom", "^16.12.0"),
      ("react-query", "^2.14.1"),
      ("react-router-dom", "^5.1.2"),
      ("react-scripts", "4.0.3"),
      ("uuid", "^3.4.0")
    ]

-- TODO: Also extract devDependencies like we did dependencies (waspNpmDeps).

generateGitignore :: Wasp -> FileDraft
generateGitignore wasp =
  C.makeTemplateFD
    (asTmplFile [relfile|gitignore|])
    (asWebAppFile [relfile|.gitignore|])
    (Just $ toJSON wasp)

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp =
  C.copyTmplAsIs (asTmplFile [relfile|public/favicon.ico|]) :
  generatePublicIndexHtml wasp :
  map
    (\path -> C.makeSimpleTemplateFD (asTmplFile $ [reldir|public|] </> path) wasp)
    [ [relfile|manifest.json|]
    ]

generatePublicIndexHtml :: Wasp -> FileDraft
generatePublicIndexHtml wasp =
  C.makeTemplateFD
    (asTmplFile [relfile|public/index.html|])
    targetPath
    (Just templateData)
  where
    targetPath = [relfile|public/index.html|]
    templateData =
      object
        [ "title" .= Wasp.App.appTitle (getApp wasp),
          "head" .= maybe "" (intercalate "\n") (Wasp.App.appHead $ getApp wasp)
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
genApi = C.copyTmplAsIs (C.asTmplFile [relfile|src/api.js|])

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp =
  generateLogo :
  RouterGenerator.generateRouter wasp :
  genApi :
  map
    makeSimpleSrcTemplateFD
    [ [relfile|index.js|],
      [relfile|index.css|],
      [relfile|serviceWorker.js|],
      [relfile|config.js|],
      [relfile|queryCache.js|]
    ]
    ++ genOperations wasp
    ++ AuthG.genAuth wasp
  where
    generateLogo =
      C.makeTemplateFD
        (asTmplFile [relfile|src/logo.png|])
        (srcDir </> asWebAppSrcFile [relfile|logo.png|])
        Nothing
    makeSimpleSrcTemplateFD path =
      C.makeTemplateFD
        (asTmplFile $ [reldir|src|] </> path)
        (srcDir </> asWebAppSrcFile path)
        (Just $ toJSON wasp)
