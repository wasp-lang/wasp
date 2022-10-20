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
    File',
    Path,
    Path',
    Posix,
    Rel,
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
import qualified Wasp.Generator.ConfigFileGenerator as CFG
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
    <++> genDotEnv spec
    <++> CFG.genWebAppConfigFiles spec

genDotEnv :: AppSpec -> Generator [FileDraft]
genDotEnv spec = return $
  case AS.dotEnvClientFile spec of
    Just srcFilePath
      | not $ AS.isBuild spec ->
          [ createCopyFileDraft
              (C.webAppRootDirInProjectRootDir </> dotEnvInWebAppRootDir)
              srcFilePath
          ]
    _ -> []

dotEnvInWebAppRootDir :: Path' (Rel C.WebAppRootDir) File'
dotEnvInWebAppRootDir = [relfile|.env|]

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
npmDepsForWasp spec =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("axios", "^0.27.2"),
            ("react", "^17.0.2"),
            ("react-dom", "^17.0.2"),
            ("react-query", "^3.39.2"),
            ("react-router-dom", "^5.3.3"),
            ("react-scripts", "5.0.1")
          ]
          ++ depsRequiredByTailwind spec,
      -- NOTE: In order to follow Create React App conventions, do not place any dependencies under devDependencies.
      -- See discussion here for more: https://github.com/wasp-lang/wasp/pull/621
      N.waspDevDependencies =
        AS.Dependency.fromList
          []
    }

depsRequiredByTailwind :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByTailwind spec =
  if CFG.isTailwindUsed spec
    then
      AS.Dependency.fromList
        [ ("tailwindcss", "^3.1.8"),
          ("postcss", "^8.4.18"),
          ("autoprefixer", "10.4.12")
        ]
    else []

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
    [ publicIndexHtmlFd,
      genFaviconFd,
      genManifestFd
    ]
      ++ genGoogleSigninImage
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFaviconFd = C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|])
    genGoogleSigninImage =
      [ C.mkTmplFd (C.asTmplFile [relfile|public/images/btn_google_signin_dark_normal_web@2x.png|])
        | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True
      ]
    genManifestFd =
      let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          tmplFile = C.asTmplFile [relfile|public/manifest.json|]
       in C.mkTmplFdWithData tmplFile tmplData

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
    [ copyTmplFile [relfile|logo.png|],
      copyTmplFile [relfile|serviceWorker.js|],
      copyTmplFile [relfile|config.js|],
      copyTmplFile [relfile|queryClient.js|],
      copyTmplFile [relfile|utils.js|],
      genRouter spec,
      genIndexJs spec,
      genIndexCss spec,
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

genIndexCss :: AppSpec -> Generator FileDraft
genIndexCss spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/index.css|])
      (C.asWebAppFile [relfile|src/index.css|])
      ( Just $
          object
            [ "isTailwindUsed" .= CFG.isTailwindUsed spec
            ]
      )
