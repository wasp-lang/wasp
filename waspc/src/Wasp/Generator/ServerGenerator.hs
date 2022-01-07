{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator
  ( genServer,
    preCleanup,
    operationsRouteInRootRouter,
    waspNpmDeps,
    waspNpmDevDeps,
  )
where

import Control.Monad (unless)
import Data.Aeson (object, (.=))
import Data.List (intercalate)
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
  )
import StrongPath (Abs, Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.CompileOptions (CompileOptions)
import Wasp.Generator.Common (ProjectRootDir, nodeVersionAsText)
import Wasp.Generator.ExternalCodeGenerator (generateExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.PackageJsonGenerator
  ( npmDepsToPackageJsonEntry,
    npmDevDepsToPackageJsonEntry,
    resolveNpmDeps,
  )
import Wasp.Generator.ServerGenerator.AuthG (genAuth)
import Wasp.Generator.ServerGenerator.Common
  ( ServerSrcDir,
    asServerFile,
    asTmplFile,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.ConfigG (genConfigFile)
import qualified Wasp.Generator.ServerGenerator.ExternalCodeGenerator as ServerExternalCodeGenerator
import Wasp.Generator.ServerGenerator.OperationsG (genOperations)
import Wasp.Generator.ServerGenerator.OperationsRoutesG (genOperationsRoutes)

genServer :: AppSpec -> [FileDraft]
genServer spec =
  concat
    [ [genReadme],
      [genPackageJson spec waspNpmDeps waspNpmDevDeps],
      [genNpmrc],
      [genNvmrc],
      [genGitignore],
      genSrcDir spec,
      generateExternalCodeDir ServerExternalCodeGenerator.generatorStrategy (AS.externalCodeFiles spec),
      genDotEnv spec
    ]

-- Cleanup to be performed before generating new server code.
-- This might be needed in case if outDir is not empty (e.g. we already generated server code there before).
-- TODO: Once we implement a fancier method of removing old/redundant files in outDir,
--   we will not need this method any more. Check https://github.com/wasp-lang/wasp/issues/209
--   for progress of this.
preCleanup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> CompileOptions -> IO ()
preCleanup _ outDir _ = do
  -- If .env gets removed but there is old .env file in generated project from previous attempts,
  -- we need to make sure we remove it.
  removeFile dotEnvAbsFilePath
    `catch` \e -> unless (isDoesNotExistError e) $ throwIO e
  where
    dotEnvAbsFilePath = SP.toFilePath $ outDir </> C.serverRootDirInProjectRootDir </> dotEnvInServerRootDir

genDotEnv :: AppSpec -> [FileDraft]
genDotEnv spec =
  case AS.dotEnvFile spec of
    Just srcFilePath ->
      [ createCopyFileDraft
          (C.serverRootDirInProjectRootDir </> dotEnvInServerRootDir)
          srcFilePath
      ]
    Nothing -> []

dotEnvInServerRootDir :: Path' (Rel C.ServerRootDir) File'
dotEnvInServerRootDir = [relfile|.env|]

genReadme :: FileDraft
genReadme = C.copyTmplAsIs (asTmplFile [relfile|README.md|])

genPackageJson :: AppSpec -> [AS.Dependency.Dependency] -> [AS.Dependency.Dependency] -> FileDraft
genPackageJson spec waspDeps waspDevDeps =
  C.makeTemplateFD
    (asTmplFile [relfile|package.json|])
    (asServerFile [relfile|package.json|])
    ( Just $
        object
          [ "depsChunk" .= npmDepsToPackageJsonEntry (resolvedWaspDeps ++ resolvedUserDeps),
            "devDepsChunk" .= npmDevDepsToPackageJsonEntry waspDevDeps,
            "nodeVersion" .= nodeVersionAsText,
            "startProductionScript"
              .= if not (null $ AS.getDecls @AS.Entity.Entity spec)
                then "npm run db-migrate-prod && "
                else
                  ""
                    ++ "NODE_ENV=production node ./src/server.js"
          ]
    )
  where
    (resolvedWaspDeps, resolvedUserDeps) =
      case resolveNpmDeps waspDeps userDeps of
        Right deps -> deps
        Left depsAndErrors -> error $ intercalate " ; " $ map snd depsAndErrors

    userDeps :: [AS.Dependency.Dependency]
    userDeps = fromMaybe [] $ AS.App.dependencies $ snd $ AS.getApp spec

waspNpmDeps :: [AS.Dependency.Dependency]
waspNpmDeps =
  AS.Dependency.fromList
    [ ("cookie-parser", "~1.4.4"),
      ("cors", "^2.8.5"),
      ("debug", "~2.6.9"),
      ("express", "~4.16.1"),
      ("morgan", "~1.9.1"),
      ("@prisma/client", "2.22.1"),
      ("jsonwebtoken", "^8.5.1"),
      ("secure-password", "^4.0.0"),
      ("dotenv", "8.2.0"),
      ("helmet", "^4.6.0")
    ]

waspNpmDevDeps :: [AS.Dependency.Dependency]
waspNpmDevDeps =
  AS.Dependency.fromList
    [ ("nodemon", "^2.0.4"),
      ("standard", "^14.3.4"),
      ("prisma", "2.22.1")
    ]

genNpmrc :: FileDraft
genNpmrc =
  C.makeTemplateFD
    (asTmplFile [relfile|npmrc|])
    (asServerFile [relfile|.npmrc|])
    Nothing

genNvmrc :: FileDraft
genNvmrc =
  C.makeTemplateFD
    (asTmplFile [relfile|nvmrc|])
    (asServerFile [relfile|.nvmrc|])
    (Just (object ["nodeVersion" .= ('v' : nodeVersionAsText)]))

genGitignore :: FileDraft
genGitignore =
  C.makeTemplateFD
    (asTmplFile [relfile|gitignore|])
    (asServerFile [relfile|.gitignore|])
    Nothing

genSrcDir :: AppSpec -> [FileDraft]
genSrcDir spec =
  concat
    [ [C.copySrcTmplAsIs $ C.asTmplSrcFile [relfile|app.js|]],
      [C.copySrcTmplAsIs $ C.asTmplSrcFile [relfile|server.js|]],
      [C.copySrcTmplAsIs $ C.asTmplSrcFile [relfile|utils.js|]],
      [C.copySrcTmplAsIs $ C.asTmplSrcFile [relfile|core/AuthError.js|]],
      [C.copySrcTmplAsIs $ C.asTmplSrcFile [relfile|core/HttpError.js|]],
      [genDbClient spec],
      [genConfigFile spec],
      genRoutesDir spec,
      genOperationsRoutes spec,
      genOperations spec,
      genAuth spec,
      [genServerJs spec]
    ]

genDbClient :: AppSpec -> FileDraft
genDbClient spec = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    maybeAuth = AS.App.auth $ snd $ AS.getApp spec

    dbClientRelToSrcP = [relfile|dbClient.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> dbClientRelToSrcP
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile dbClientRelToSrcP

    tmplData =
      if isJust maybeAuth
        then
          object
            [ "isAuthEnabled" .= True,
              "userEntityUpper" .= AS.Core.Ref.refName (AS.App.Auth.userEntity $ fromJust maybeAuth)
            ]
        else object []

genServerJs :: AppSpec -> FileDraft
genServerJs spec =
  C.makeTemplateFD
    (asTmplFile [relfile|src/server.js|])
    (asServerFile [relfile|src/server.js|])
    ( Just $
        object
          [ "doesServerSetupFnExist" .= isJust maybeSetupJsFunction,
            "serverSetupJsFnImportStatement" .= fromMaybe "" maybeSetupJsFnImportStmt,
            "serverSetupJsFnIdentifier" .= fromMaybe "" maybeSetupJsFnImportIdentifier
          ]
    )
  where
    maybeSetupJsFunction = AS.App.Server.setupFn =<< AS.App.server (snd $ AS.getApp spec)
    maybeSetupJsFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromSrcDirToExtSrcDir <$> maybeSetupJsFunction
    (maybeSetupJsFnImportIdentifier, maybeSetupJsFnImportStmt) =
      (fst <$> maybeSetupJsFnImportDetails, snd <$> maybeSetupJsFnImportDetails)

-- | TODO: Make this not hardcoded!
relPosixPathFromSrcDirToExtSrcDir :: Path Posix (Rel (Dir ServerSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromSrcDirToExtSrcDir = [reldirP|./ext-src|]

genRoutesDir :: AppSpec -> [FileDraft]
genRoutesDir spec =
  -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
  -- but I did not bother with it yet since it is used only here for now.
  [ C.makeTemplateFD
      (asTmplFile [relfile|src/routes/index.js|])
      (asServerFile [relfile|src/routes/index.js|])
      ( Just $
          object
            [ "operationsRouteInRootRouter" .= operationsRouteInRootRouter,
              -- TODO: This is commonly used (isAuthEnabled), maybe I should extract this logic
              -- so it can be reused? Put it somewhere in AppSpec? Or in Generator? Probably AppSpec.
              -- I could also extract some other stuff like getAuth, getQueries, getActions, ... .
              "isAuthEnabled" .= isJust (AS.App.auth $ snd $ AS.getApp spec)
            ]
      )
  ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"
