{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common
  ( makeJsonWithEntityData,
    nodeVersionRange,
    prismaVersion,
  )
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.ExternalAuthG (ExternalAuthInfo (..), gitHubAuthInfo, googleAuthInfo)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator
  ( extClientCodeGeneratorStrategy,
    extSharedCodeGeneratorStrategy,
  )
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.OperationsGenerator (genOperations)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|tsconfig.json|],
      genFileCopy [relfile|tsconfig.node.json|],
      genFileCopy [relfile|vite.config.ts|],
      genFileCopy [relfile|netlify.toml|],
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore,
      genIndexHtml spec
    ]
    <++> genPublicDir spec
    <++> genSrcDir spec
    <++> genExternalCodeDir extClientCodeGeneratorStrategy (AS.externalClientFiles spec)
    <++> genExternalCodeDir extSharedCodeGeneratorStrategy (AS.externalSharedFiles spec)
    <++> genDotEnv spec
    <++> genUniversalDir
    <++> genEnvValidationScript
  where
    genFileCopy = return . C.mkTmplFd

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
              "nodeVersionRange" .= show nodeVersionRange
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
            ("@tanstack/react-query", "^4.13.0"),
            ("react-router-dom", "^5.3.3"),
            -- The web app only needs @prisma/client (we're using the server's
            -- CLI to generate what's necessary, check the description in
            -- https://github.com/wasp-lang/wasp/pull/962/ for details).
            ("@prisma/client", show prismaVersion)
          ]
          ++ depsRequiredByTailwind spec,
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("vite", "^4.1.0"),
            ("typescript", "^4.9.3"),
            ("@types/react", "^17.0.53"),
            ("@types/react-dom", "^17.0.19"),
            ("@types/react-router-dom", "^5.3.3"),
            ("@vitejs/plugin-react-swc", "^3.0.0"),
            ("dotenv", "^16.0.3"),
            -- NOTE: Make sure to bump the version of the tsconfig
            -- when updating Vite or React versions
            ("@tsconfig/vite-react", "^1.0.1")
          ]
    }

depsRequiredByTailwind :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByTailwind spec =
  if G.CF.isTailwindUsed spec
    then
      AS.Dependency.fromList
        [ ("tailwindcss", "^3.2.7"),
          ("postcss", "^8.4.21"),
          ("autoprefixer", "^10.4.13")
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
  return
    [ genFaviconFd,
      genManifestFd
    ]
    <++> genSocialLoginIcons maybeAuth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFaviconFd = C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|])
    genManifestFd =
      let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          tmplFile = C.asTmplFile [relfile|public/manifest.json|]
       in C.mkTmplFdWithData tmplFile tmplData

genSocialLoginIcons :: Maybe AS.App.Auth.Auth -> Generator [FileDraft]
genSocialLoginIcons maybeAuth =
  return $
    [ C.mkTmplFd (C.asTmplFile fp)
      | (isEnabled, fp) <- socialIcons,
        (isEnabled <$> maybeAuth) == Just True
    ]
  where
    socialIcons =
      [ (AS.App.Auth.isGoogleAuthEnabled, [reldir|public/images|] </> _logoFileName googleAuthInfo),
        (AS.App.Auth.isGitHubAuthEnabled, [reldir|public/images|] </> _logoFileName gitHubAuthInfo)
      ]

genIndexHtml :: AppSpec -> Generator FileDraft
genIndexHtml spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|index.html|])
      targetPath
      (Just templateData)
  where
    targetPath = [relfile|index.html|]
    templateData =
      object
        [ "title" .= (AS.App.title (snd $ getApp spec) :: String),
          "head" .= (maybe "" (intercalate "\n") (AS.App.head $ snd $ getApp spec) :: String)
        ]

-- TODO(matija): Currently we also generate auth-specific parts in this file (e.g. token management),
-- although they are not used anywhere outside.
-- We could further "templatize" this file so only what is needed is generated.
genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ copyTmplFile [relfile|logo.png|],
      copyTmplFile [relfile|config.js|],
      copyTmplFile [relfile|queryClient.js|],
      copyTmplFile [relfile|utils.js|],
      copyTmplFile [relfile|vite-env.d.ts|],
      -- Generates api.js file which contains token management and configured api (e.g. axios) instance.
      copyTmplFile [relfile|api.ts|],
      copyTmplFile [relfile|storage.ts|],
      genRouter spec,
      genIndexJs spec
    ]
    <++> genOperations spec
    <++> genEntitiesDir spec
    <++> genAuth spec
  where
    copyTmplFile = return . C.mkSrcTmplFd

genEntitiesDir :: AppSpec -> Generator [FileDraft]
genEntitiesDir spec = return [entitiesIndexFileDraft]
  where
    entitiesIndexFileDraft =
      C.mkTmplFdWithDstAndData
        [relfile|src/entities/index.ts|]
        [relfile|src/entities/index.ts|]
        (Just $ object ["entities" .= allEntities])
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getDecls @AS.Entity.Entity spec

genIndexJs :: AppSpec -> Generator FileDraft
genIndexJs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/index.tsx|])
      (C.asWebAppFile [relfile|src/index.tsx|])
      ( Just $
          object
            [ "setupFn" .= extImportToImportJson relPathToWebAppSrcDir maybeSetupJsFunction
            ]
      )
  where
    maybeSetupJsFunction = AS.App.Client.setupFn =<< AS.App.client (snd $ getApp spec)

    relPathToWebAppSrcDir :: Path Posix (Rel importLocation) (Dir C.WebAppSrcDir)
    relPathToWebAppSrcDir = [reldirP|./|]

genUniversalDir :: Generator [FileDraft]
genUniversalDir =
  return
    [ C.mkUniversalTmplFdWithDst [relfile|url.ts|] [relfile|src/universal/url.ts|]
    ]

genEnvValidationScript :: Generator [FileDraft]
genEnvValidationScript =
  return
    [ C.mkTmplFd [relfile|scripts/validate-env.mjs|],
      C.mkUniversalTmplFdWithDst [relfile|validators.js|] [relfile|scripts/universal/validators.mjs|]
    ]
