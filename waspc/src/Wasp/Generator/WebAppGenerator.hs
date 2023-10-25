{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    npmDepsForWasp,
  )
where

import Data.Aeson (object, (.=))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromJust)
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
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.App (App (webSocket))
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.App.WebSocket (WebSocket (..))
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Env (envVarsToDotEnvContent)
import Wasp.Generator.Common
  ( makeJsonWithEntityData,
    prismaVersion,
  )
import qualified Wasp.Generator.ConfigFile as G.CF
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import qualified Wasp.Generator.ExternalCodeGenerator.Common as ECC
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.WebAppGenerator.AuthG (genAuth)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.CrudG (genCrud)
import Wasp.Generator.WebAppGenerator.ExternalCodeGenerator
  ( extClientCodeGeneratorStrategy,
    extSharedCodeGeneratorStrategy,
  )
import qualified Wasp.Generator.WebAppGenerator.ExternalCodeGenerator as EC
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.WebAppGenerator.OperationsGenerator (genOperations)
import Wasp.Generator.WebAppGenerator.RouterGenerator (genRouter)
import qualified Wasp.Generator.WebSocket as AS.WS
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportModule),
    makeJsImport,
  )
import qualified Wasp.Node.Version as NodeVersion
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec = do
  extClientCodeFileDrafts <- genExternalCodeDir extClientCodeGeneratorStrategy (AS.externalClientFiles spec)
  sequence
    [ genFileCopy [relfile|README.md|],
      genFileCopy [relfile|tsconfig.json|],
      genFileCopy [relfile|tsconfig.node.json|],
      genFileCopy [relfile|src/test/vitest/setup.ts|],
      genFileCopy [relfile|src/test/vitest/helpers.tsx|],
      genFileCopy [relfile|src/test/index.ts|],
      genFileCopy [relfile|netlify.toml|],
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore,
      genIndexHtml spec,
      genViteConfig spec
    ]
    <++> genSrcDir spec
    <++> return extClientCodeFileDrafts
    <++> genExternalCodeDir extSharedCodeGeneratorStrategy (AS.externalSharedFiles spec)
    <++> genPublicDir spec extClientCodeFileDrafts
    <++> genDotEnv spec
    <++> genUniversalDir
    <++> genEnvValidationScript
    <++> genCrud spec
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
          [ ("axios", "^1.4.0"),
            ("react", "^18.2.0"),
            ("react-dom", "^18.2.0"),
            ("@tanstack/react-query", "^4.29.0"),
            ("react-router-dom", "^5.3.3"),
            -- The web app only needs @prisma/client (we're using the server's
            -- CLI to generate what's necessary, check the description in
            -- https://github.com/wasp-lang/wasp/pull/962/ for details).
            ("@prisma/client", show prismaVersion),
            ("superjson", "^1.12.2"),
            ("mitt", "3.0.0"),
            -- Used for Auth UI
            ("react-hook-form", "^7.45.4")
          ]
          ++ depsRequiredForAuth spec
          ++ depsRequiredByTailwind spec
          ++ depsRequiredForWebSockets spec,
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ -- TODO: Allow users to choose whether they want to use TypeScript
            -- in their projects and install these dependencies accordingly.
            ("vite", "^4.3.9"),
            ("typescript", "^5.1.0"),
            ("@types/react", "^18.0.37"),
            ("@types/react-dom", "^18.0.11"),
            ("@types/react-router-dom", "^5.3.3"),
            ("@vitejs/plugin-react-swc", "^3.0.0"),
            ("dotenv", "^16.0.3"),
            -- NOTE: Make sure to bump the version of the tsconfig
            -- when updating Vite or React versions
            ("@tsconfig/vite-react", "^2.0.0")
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
      ("@testing-library/react", "^14.0.0"),
      ("@testing-library/jest-dom", "^5.16.5"),
      ("msw", "^1.1.0")
    ]

depsRequiredForWebSockets :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredForWebSockets spec
  | AS.WS.areWebSocketsUsed spec = AS.WS.clientDepsRequiredForWebSockets
  | otherwise = []

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDst
      (C.asTmplFile [relfile|gitignore|])
      (C.asWebAppFile [relfile|.gitignore|])

genPublicDir :: AppSpec -> [FileDraft] -> Generator [FileDraft]
genPublicDir spec extCodeFileDrafts =
  return $
    ifUserDidntProvideFile genFaviconFd
      ++ ifUserDidntProvideFile genManifestFd
  where
    genFaviconFd = C.mkTmplFd (C.asTmplFile [relfile|public/favicon.ico|])
    genManifestFd = C.mkTmplFdWithData tmplFile tmplData
      where
        tmplData = object ["appName" .= (fst (getApp spec) :: String)]
        tmplFile = C.asTmplFile [relfile|public/manifest.json|]

    ifUserDidntProvideFile fileDraft =
      if checkIfFileDraftExists fileDraft
        then []
        else [fileDraft]

    checkIfFileDraftExists = (`elem` existingDstPaths) . FD.getDstPath
    existingDstPaths = map FD.getDstPath extCodeFileDrafts

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
      genFileCopy [relfile|api/events.ts|],
      genFileCopy [relfile|storage.ts|],
      getIndexTs spec
    ]
    <++> genOperations spec
    <++> genEntitiesDir spec
    <++> genAuth spec
    <++> genWebSockets spec
    <++> genRouter spec
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

getIndexTs :: AppSpec -> Generator FileDraft
getIndexTs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/index.tsx|])
      (C.asWebAppFile [relfile|src/index.tsx|])
      ( Just $
          object
            [ "setupFn" .= extImportToImportJson relPathToWebAppSrcDir maybeSetupJsFunction,
              "areWebSocketsUsed" .= AS.WS.areWebSocketsUsed spec
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

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec =
      sequence
        [ genFileCopy [relfile|webSocket.ts|],
          genWebSocketProvider spec
        ]
  | otherwise = return []
  where
    genFileCopy = return . C.mkSrcTmplFd

genWebSocketProvider :: AppSpec -> Generator FileDraft
genWebSocketProvider spec = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    maybeWebSocket = webSocket $ snd $ getApp spec
    shouldAutoConnect = (autoConnect <$> maybeWebSocket) /= Just (Just False)
    tmplData = object ["autoConnect" .= map toLower (show shouldAutoConnect)]
    tmplFile = C.asTmplFile [relfile|src/webSocket/WebSocketProvider.tsx|]

genViteConfig :: AppSpec -> Generator FileDraft
genViteConfig spec = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = C.asTmplFile [relfile|vite.config.ts|]
    tmplData =
      object
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec),
          "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "defaultClientPort" .= C.defaultClientPort
        ]

    makeCustomViteConfigJsImport :: Path' (Rel SourceExternalCodeDir) File' -> JsImport
    makeCustomViteConfigJsImport pathToConfig = makeJsImport importPath importName
      where
        importPath = C.toViteImportPath $ fromJust $ SP.relFileToPosix pathToConfigInSrc
        pathToConfigInSrc =
          SP.castRel $
            C.webAppSrcDirInWebAppRootDir
              </> EC.extClientCodeDirInWebAppSrcDir
              </> ECC.castRelPathFromSrcToGenExtCodeDir pathToConfig

        importName = JsImportModule "customViteConfig"
