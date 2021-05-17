module Generator.WebAppGenerator
  ( generateWebApp,
    waspNpmDeps,
  )
where

import CompileOptions (CompileOptions)
import Data.Aeson
  ( ToJSON (..),
    object,
    (.=),
  )
import Data.List (intercalate)
import Generator.ExternalCodeGenerator (generateExternalCodeDir)
import Generator.FileDraft
import Generator.PackageJsonGenerator
  ( resolveNpmDeps,
    toPackageJsonDependenciesString,
  )
import qualified Generator.WebAppGenerator.AuthG as AuthG
import Generator.WebAppGenerator.Common
  ( asTmplFile,
    asWebAppFile,
    asWebAppSrcFile,
  )
import qualified Generator.WebAppGenerator.Common as C
import qualified Generator.WebAppGenerator.ExternalCodeGenerator as WebAppExternalCodeGenerator
import Generator.WebAppGenerator.OperationsGenerator (genOperations)
import qualified Generator.WebAppGenerator.RouterGenerator as RouterGenerator
import qualified NpmDependency as ND
import qualified Path as P
import StrongPath
  ( Dir,
    Path,
    Rel,
    (</>),
  )
import qualified StrongPath as SP
import Wasp
import qualified Wasp.App
import qualified Wasp.NpmDependencies as WND

generateWebApp :: Wasp -> CompileOptions -> [FileDraft]
generateWebApp wasp _ =
  concat
    [ [generateReadme wasp],
      [genPackageJson wasp waspNpmDeps],
      [generateGitignore wasp],
      generatePublicDir wasp,
      generateSrcDir wasp,
      generateExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy wasp,
      [C.makeSimpleTemplateFD (asTmplFile [P.relfile|netlify.toml|]) wasp]
    ]

generateReadme :: Wasp -> FileDraft
generateReadme wasp = C.makeSimpleTemplateFD (asTmplFile [P.relfile|README.md|]) wasp

genPackageJson :: Wasp -> [ND.NpmDependency] -> FileDraft
genPackageJson wasp waspDeps =
  C.makeTemplateFD
    (C.asTmplFile [P.relfile|package.json|])
    (C.asWebAppFile [P.relfile|package.json|])
    ( Just $
        object
          [ "wasp" .= wasp,
            "depsChunk" .= toPackageJsonDependenciesString (resolvedWaspDeps ++ resolvedUserDeps)
          ]
    )
  where
    (resolvedWaspDeps, resolvedUserDeps) =
      case resolveNpmDeps waspDeps userDeps of
        Right deps -> deps
        Left depsAndErrors -> error $ intercalate " ; " $ map snd depsAndErrors

    userDeps :: [ND.NpmDependency]
    userDeps = WND._dependencies $ Wasp.getNpmDependencies wasp

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
    (asTmplFile [P.relfile|gitignore|])
    (asWebAppFile [P.relfile|.gitignore|])
    (Just $ toJSON wasp)

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp =
  C.copyTmplAsIs (asTmplFile [P.relfile|public/favicon.ico|]) :
  generatePublicIndexHtml wasp :
  map
    (\path -> C.makeSimpleTemplateFD (asTmplFile $ [P.reldir|public|] P.</> path) wasp)
    [ [P.relfile|manifest.json|]
    ]

generatePublicIndexHtml :: Wasp -> FileDraft
generatePublicIndexHtml wasp =
  C.makeTemplateFD
    (asTmplFile $ [P.relfile|public/index.html|])
    targetPath
    (Just templateData)
  where
    targetPath = SP.fromPathRelFile [P.relfile|public/index.html|]
    templateData =
      object
        [ "title" .= (Wasp.App.appTitle $ getApp wasp),
          "head" .= (maybe "" (intercalate "\n") (Wasp.App.appHead $ getApp wasp))
        ]

-- * Src dir

srcDir :: Path (Rel C.WebAppRootDir) (Dir C.WebAppSrcDir)
srcDir = C.webAppSrcDirInWebAppRootDir

-- TODO(matija): Currently we also generate auth-specific parts in this file (e.g. token management),
-- although they are not used anywhere outside.
-- We could further "templatize" this file so only what is needed is generated.
--

-- | Generates api.js file which contains token management and configured api (e.g. axios) instance.
genApi :: FileDraft
genApi = C.copyTmplAsIs (C.asTmplFile [P.relfile|src/api.js|])

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp =
  generateLogo :
  RouterGenerator.generateRouter wasp :
  genApi :
  map
    makeSimpleSrcTemplateFD
    [ [P.relfile|index.js|],
      [P.relfile|index.css|],
      [P.relfile|serviceWorker.js|],
      [P.relfile|config.js|],
      [P.relfile|queryCache.js|]
    ]
    ++ genOperations wasp
    ++ AuthG.genAuth wasp
  where
    generateLogo =
      C.makeTemplateFD
        (asTmplFile [P.relfile|src/logo.png|])
        (srcDir </> asWebAppSrcFile [P.relfile|logo.png|])
        Nothing
    makeSimpleSrcTemplateFD path =
      C.makeTemplateFD
        (asTmplFile $ [P.reldir|src|] P.</> path)
        (srcDir </> asWebAppSrcFile path)
        (Just $ toJSON wasp)
