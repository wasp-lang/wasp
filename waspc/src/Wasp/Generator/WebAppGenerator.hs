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
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (nodeVersionRange, npmVersionRange)
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.ExternalCodeGenerator as WebAppExternalCodeGenerator
import Wasp.Generator.WebAppGenerator.OperationsGenerator (genOperations)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genReadme,
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore,
      return $ C.mkTmplFd $ C.asTmplFile [relfile|netlify.toml|]
    ]
    <++> genPublicDir spec
    <++> genSrcDir spec
    <++> genExternalCodeDir WebAppExternalCodeGenerator.generatorStrategy (AS.externalCodeFiles spec)

genReadme :: Generator FileDraft
genReadme = return $ C.mkTmplFd $ C.asTmplFile [relfile|README.md|]

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
              "nodeVersionRange" .= show nodeVersionRange,
              "npmVersionRange" .= show npmVersionRange
            ]
      )

genNpmrc :: Generator FileDraft
genNpmrc =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|npmrc|])
      (C.asWebAppFile [relfile|.npmrc|])
      Nothing

npmDepsForWasp :: AppSpec -> N.NpmDepsForWasp
npmDepsForWasp _spec =
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
            -- NOTE: We need to specify this exact version of `react-error-overlay` for use with
            -- `react-scripts` v4 due to this issue: https://github.com/facebook/create-react-app/issues/11773
            ("react-error-overlay", "6.0.9")
          ],
      -- NOTE: In order to follow Create React App conventions, do not place any dependencies under devDependencies.
      -- See discussion here for more: https://github.com/wasp-lang/wasp/pull/621
      N.waspDevDependencies =
        AS.Dependency.fromList
          []
    }

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDst
      (C.asTmplFile [relfile|gitignore|])
      (C.asWebAppFile [relfile|.gitignore|])

genPublicDir :: AppSpec -> Generator [FileDraft]
genPublicDir spec = do
  publicIndexHtmlFd <- genPublicIndexHtml spec
  return $
    C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|]) :
    publicIndexHtmlFd :
    ( let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          processPublicTmpl path = C.mkTmplFdWithData (C.asTmplFile $ [reldir|public|] </> path) tmplData
       in processPublicTmpl [relfile|manifest.json|]
    ) :
      [C.mkTmplFd (C.asTmplFile [relfile|public/images/btn_google_signin_dark_normal_web@2x.png|]) | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genPublicIndexHtml :: AppSpec -> Generator FileDraft
genPublicIndexHtml spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|public/index.html|])
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
  sequence
    [ copyTmplFile [relfile|index.css|],
      copyTmplFile [relfile|logo.png|],
      copyTmplFile [relfile|serviceWorker.js|],
      copyTmplFile [relfile|config.js|],
      copyTmplFile [relfile|queryClient.js|],
      copyTmplFile [relfile|utils.js|],
      genRouter spec,
      genIndexJs spec,
      genApi
    ]
    <++> genOperations spec
    <++> genAuth spec
  where
    copyTmplFile = return . C.mkSrcTmplFd

-- | Generates api.js file which contains token management and configured api (e.g. axios) instance.
genApi :: Generator FileDraft
genApi = return $ C.mkTmplFd (C.asTmplFile [relfile|src/api.js|])

genIndexJs :: AppSpec -> Generator FileDraft
genIndexJs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/index.js|])
      (C.asWebAppFile [relfile|src/index.js|])
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

relPosixPathFromSrcDirToExtSrcDir :: Path Posix (Rel (Dir C.WebAppSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromSrcDirToExtSrcDir = [reldirP|./ext-src|]
