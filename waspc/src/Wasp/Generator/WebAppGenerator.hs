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
    reldirP,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Env (envVarsToDotEnvContent)
import Wasp.Generator.Common
  ( makeJsonWithEntityData,
    prismaVersion,
  )
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import Wasp.Generator.FileDraft
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator
  ( extClientCodeGeneratorStrategy,
    extSharedCodeGeneratorStrategy,
  )
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.OperationsGenerator (genOperations)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import qualified Wasp.Node.Version as NodeVersion
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|tsconfig.json|],
      genFileCopy [relfile|tsconfig.node.json|],
      genFileCopy [relfile|vite.config.ts|],
      genFileCopy [relfile|src/test/vitest/setup.ts|],
      genFileCopy [relfile|src/test/vitest/helpers.tsx|],
      genFileCopy [relfile|src/test/index.ts|],
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
-- Don't generate .env if we are building for production, since .env is to be used only for
-- development.
genDotEnv spec | AS.isBuild spec = return []
genDotEnv spec =
  return
    [ createTextFileDraft
        (C.webAppRootDirInProjectRootDir </> dotEnvInWebAppRootDir)
        (envVarsToDotEnvContent $ AS.devEnvVarsClient spec)
    ]

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
              "nodeVersionRange" .= show NodeVersion.nodeVersionRange
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
            ("@prisma/client", show prismaVersion),
            ("superjson", "^1.12.2")
          ]
          ++ depsRequiredForAuth spec
          ++ depsRequiredByTailwind spec,
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("vite", "^4.1.0"),
            ("typescript", "^5.0.0"),
            ("@types/react", "^17.0.53"),
            ("@types/react-dom", "^17.0.19"),
            ("@types/react-router-dom", "^5.3.3"),
            ("@vitejs/plugin-react-swc", "^3.0.0"),
            ("dotenv", "^16.0.3"),
            -- NOTE: Make sure to bump the version of the tsconfig
            -- when updating Vite or React versions
            ("@tsconfig/vite-react", "^1.0.1")
          ]
          ++ depsRequiredForTesting
    }

depsRequiredForAuth :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredForAuth spec =
  [AS.Dependency.make ("@stitches/react", show versionRange) | isAuthEnabled spec]
  where
    versionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 1 2 8)]

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

depsRequiredForTesting :: [AS.Dependency.Dependency]
depsRequiredForTesting =
  AS.Dependency.fromList
    [ ("vitest", "^0.29.3"),
      ("@vitest/ui", "^0.29.3"),
      ("jsdom", "^21.1.1"),
      ("@testing-library/react", "^12.1.5"),
      ("@testing-library/jest-dom", "^5.16.5"),
      ("msw", "^1.1.0")
    ]

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
  where
    genFaviconFd = C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|])
    genManifestFd =
      let tmplData = object ["appName" .= (fst (getApp spec) :: String)]
          tmplFile = C.asTmplFile [relfile|public/manifest.json|]
       in C.mkTmplFdWithData tmplFile tmplData

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
    [ genFileCopy [relfile|logo.png|],
      genFileCopy [relfile|config.js|],
      genFileCopy [relfile|queryClient.js|],
      genFileCopy [relfile|utils.js|],
      genFileCopy [relfile|types.ts|],
      genFileCopy [relfile|vite-env.d.ts|],
      -- Generates api.js file which contains token management and configured api (e.g. axios) instance.
      genFileCopy [relfile|api.ts|],
      genFileCopy [relfile|storage.ts|],
      genRouter spec,
      genIndexJs spec
    ]
    <++> genOperations spec
    <++> genEntitiesDir spec
    <++> genAuth spec
  where
    genFileCopy = return . C.mkSrcTmplFd

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
    [ C.mkUniversalTmplFdWithDst [relfile|url.ts|] [relfile|src/universal/url.ts|],
      C.mkUniversalTmplFdWithDst [relfile|types.ts|] [relfile|src/universal/types.ts|]
    ]

genEnvValidationScript :: Generator [FileDraft]
genEnvValidationScript =
  return
    [ C.mkTmplFd [relfile|scripts/validate-env.mjs|],
      C.mkUniversalTmplFdWithDst [relfile|validators.js|] [relfile|scripts/universal/validators.mjs|]
    ]
