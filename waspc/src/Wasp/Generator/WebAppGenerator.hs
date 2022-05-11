module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import StrongPath
  ( Dir,
    Path,
    Posix,
    Rel,
    reldir,
    relfile,
    (</>),
  )
import StrongPath.TH (reldirP)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (nodeVersion, nodeVersionBounds, npmVersionBounds)
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.WebAppGenerator.AuthG as AuthG
import Wasp.Generator.WebAppGenerator.Common
  ( WebAppSrcDir,
    asTmplFile,
    asWebAppFile,
    mkSrcTmplFd,
  )
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.ExternalCodeGenerator as WebAppExternalCodeGenerator
import qualified Wasp.Generator.WebAppGenerator.OperationsGenerator as OperationsG
import qualified Wasp.Generator.WebAppGenerator.RouterGenerator as RouterGenerator
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genReadme,
      genPackageJson spec npmDepsForWasp,
      genNpmrc,
      genNvmrc,
      genGitignore,
      return $ C.mkTmplFd $ asTmplFile [relfile|netlify.toml|]
    ]
    <++> genPublicDir spec
    <++> genSrcDir spec
    <++> genExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy (AS.externalCodeFiles spec)

genReadme :: Generator FileDraft
genReadme = return $ C.mkTmplFd $ asTmplFile [relfile|README.md|]

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
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry combinedDependencies,
              "nodeVersionBounds" .= show nodeVersionBounds,
              "npmVersionBounds" .= show npmVersionBounds
            ]
      )

genNpmrc :: Generator FileDraft
genNpmrc =
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile [relfile|npmrc|])
      (asWebAppFile [relfile|.npmrc|])
      Nothing

genNvmrc :: Generator FileDraft
genNvmrc =
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile [relfile|nvmrc|])
      (asWebAppFile [relfile|.nvmrc|])
      -- We want to specify only the major version, check the comment in `ServerGenerator.hs` for details
      (Just (object ["nodeVersion" .= show (SV.major nodeVersion)]))

npmDepsForWasp :: N.NpmDepsForWasp
npmDepsForWasp =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("axios", "^0.21.1"),
            ("lodash", "^4.17.15"),
            ("react", "^16.12.0"),
            ("react-dom", "^16.12.0"),
            ("react-query", "^3.34.19"),
            ("react-router-dom", "^5.1.2"),
            ("react-scripts", "4.0.3"),
            ("uuid", "^3.4.0")
          ],
      N.waspDevDependencies =
        AS.Dependency.fromList
          []
    }

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDst
      (asTmplFile [relfile|gitignore|])
      (asWebAppFile [relfile|.gitignore|])

genPublicDir :: AppSpec -> Generator [FileDraft]
genPublicDir spec = do
  publicIndexHtmlFd <- genPublicIndexHtml spec
  return $
    C.mkTmplFd (asTmplFile [relfile|public/favicon.ico|]) :
    publicIndexHtmlFd :
    ( let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          processPublicTmpl path = C.mkTmplFdWithData (asTmplFile $ [reldir|public|] </> path) tmplData
       in processPublicTmpl
            <$> [ [relfile|manifest.json|]
                ]
    )

genPublicIndexHtml :: AppSpec -> Generator FileDraft
genPublicIndexHtml spec =
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

-- TODO(matija): Currently we also generate auth-specific parts in this file (e.g. token management),
-- although they are not used anywhere outside.
-- We could further "templatize" this file so only what is needed is generated.
--

genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence [genRouter, genIndexJs spec, genApi]
    <++> genDirectCopies
    <++> genOperationsDir
    <++> genAuthDir
  where
    genRouter = RouterGenerator.generateRouter spec
    genOperationsDir = OperationsG.genOperations spec
    genAuthDir = AuthG.genAuth spec

genDirectCopies :: Generator [FileDraft]
genDirectCopies =
  return $
    map
      mkSrcTmplFd
      [ [relfile|index.css|],
        [relfile|logo.png|],
        [relfile|serviceWorker.js|],
        [relfile|config.js|],
        [relfile|queryClient.js|],
        [relfile|utils.js|]
      ]

-- | Generates api.js file which contains token management and configured api (e.g. axios) instance.
genApi :: Generator FileDraft
genApi = return $ C.mkTmplFd (C.asTmplFile [relfile|src/api.js|])

-- | TODO(filip): we have almost the same thing in server
genIndexJs :: AppSpec -> Generator FileDraft
genIndexJs spec =
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile [relfile|src/index.js|])
      (asWebAppFile [relfile|src/index.js|])
      ( Just $
          object
            [ "doesClientSetupFnExist" .= isJust maybeSetupJsFunction,
              "clientSetupJsFnImportStatement" .= fromMaybe "" maybeSetupJsFnImportStmt,
              "clientSetupJsFnIdentifier" .= fromMaybe "" maybeSetupJsFnImportIdentifier
            ]
      )
  where
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)
    maybeSetupJsFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromSrcDirToExtSrcDir <$> maybeSetupJsFunction
    (maybeSetupJsFnImportIdentifier, maybeSetupJsFnImportStmt) =
      (fst <$> maybeSetupJsFnImportDetails, snd <$> maybeSetupJsFnImportDetails)

-- | TODO(filip): Move this somewhere common (we have the same thing in server) 
relPosixPathFromSrcDirToExtSrcDir :: Path Posix (Rel (Dir WebAppSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromSrcDirToExtSrcDir = [reldirP|./ext-src|]