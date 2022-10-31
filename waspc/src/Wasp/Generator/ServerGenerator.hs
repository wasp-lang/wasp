{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator
  ( genServer,
    operationsRouteInRootRouter,
    npmDepsForWasp,
    areServerPatchesUsed,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
  ( fromJust,
    fromMaybe,
    isJust,
  )
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    relDirToPosix,
    reldir,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (nodeVersionRange, npmVersionRange, prismaVersion)
import Wasp.Generator.ExternalCodeGenerator (genExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.ServerGenerator.AuthG (genAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.ConfigG (genConfigFile)
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir, extServerCodeGeneratorStrategy, extSharedCodeGeneratorStrategy)
import Wasp.Generator.ServerGenerator.JobGenerator (depsRequiredByJobs, genJobExecutors, genJobs)
import Wasp.Generator.ServerGenerator.OperationsG (genOperations)
import Wasp.Generator.ServerGenerator.OperationsRoutesG (genOperationsRoutes)
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  sequence
    [ genReadme,
      genPackageJson spec (npmDepsForWasp spec),
      genNpmrc,
      genGitignore
    ]
    <++> genSrcDir spec
    <++> genExternalCodeDir extServerCodeGeneratorStrategy (AS.externalServerFiles spec)
    <++> genExternalCodeDir extSharedCodeGeneratorStrategy (AS.externalSharedFiles spec)
    <++> genDotEnv spec
    <++> genJobs spec
    <++> genJobExecutors
    <++> genPatches spec

genDotEnv :: AppSpec -> Generator [FileDraft]
genDotEnv spec = return $
  case AS.dotEnvServerFile spec of
    Just srcFilePath
      | not $ AS.isBuild spec ->
          [ createCopyFileDraft
              (C.serverRootDirInProjectRootDir </> dotEnvInServerRootDir)
              srcFilePath
          ]
    _ -> []

dotEnvInServerRootDir :: Path' (Rel C.ServerRootDir) File'
dotEnvInServerRootDir = [relfile|.env|]

genReadme :: Generator FileDraft
genReadme = return $ C.mkTmplFd (C.asTmplFile [relfile|README.md|])

genPackageJson :: AppSpec -> N.NpmDepsForWasp -> Generator FileDraft
genPackageJson spec waspDependencies = do
  combinedDependencies <- N.genNpmDepsForPackage spec waspDependencies
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|package.json|])
      (C.asServerFile [relfile|package.json|])
      ( Just $
          object
            [ "depsChunk" .= N.getDependenciesPackageJsonEntry combinedDependencies,
              "devDepsChunk" .= N.getDevDependenciesPackageJsonEntry combinedDependencies,
              "nodeVersionRange" .= show nodeVersionRange,
              "npmVersionRange" .= show npmVersionRange,
              "startProductionScript"
                .= ( (if hasEntities then "npm run db-migrate-prod && " else "")
                       ++ "NODE_ENV=production node ./src/server.js"
                   ),
              "overrides" .= getPackageJsonOverrides
            ]
      )
  where
    hasEntities = not . null $ AS.getDecls @AS.Entity.Entity spec

npmDepsForWasp :: AppSpec -> N.NpmDepsForWasp
npmDepsForWasp spec =
  N.NpmDepsForWasp
    { N.waspDependencies =
        AS.Dependency.fromList
          [ ("cookie-parser", "~1.4.6"),
            ("cors", "^2.8.5"),
            ("debug", "~4.3.4"),
            ("express", "~4.18.1"),
            ("morgan", "~1.10.0"),
            ("@prisma/client", show prismaVersion),
            ("jsonwebtoken", "^8.5.1"),
            -- NOTE: secure-password has a package.json override for sodium-native.
            ("secure-password", "^4.0.0"),
            ("dotenv", "16.0.2"),
            ("helmet", "^6.0.0"),
            ("patch-package", "^6.4.7"),
            ("uuid", "^9.0.0"),
            ("lodash", "^4.17.21")
          ]
          ++ depsRequiredByPassport spec
          ++ depsRequiredByJobs spec,
      N.waspDevDependencies =
        AS.Dependency.fromList
          [ ("nodemon", "^2.0.19"),
            ("standard", "^17.0.0"),
            ("prisma", show prismaVersion)
          ]
    }

genNpmrc :: Generator FileDraft
genNpmrc =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|npmrc|])
      (C.asServerFile [relfile|.npmrc|])
      Nothing

genGitignore :: Generator FileDraft
genGitignore =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|gitignore|])
      (C.asServerFile [relfile|.gitignore|])
      Nothing

genSrcDir :: AppSpec -> Generator [FileDraft]
genSrcDir spec =
  sequence
    [ copyTmplFile [relfile|app.js|],
      copyTmplFile [relfile|utils.js|],
      copyTmplFile [relfile|core/AuthError.js|],
      copyTmplFile [relfile|core/HttpError.js|],
      genDbClient spec,
      genConfigFile spec,
      genServerJs spec
    ]
    <++> genRoutesDir spec
    <++> genOperationsRoutes spec
    <++> genOperations spec
    <++> genAuth spec
  where
    copyTmplFile = return . C.mkSrcTmplFd

genDbClient :: AppSpec -> Generator FileDraft
genDbClient spec = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

    dbClientRelToSrcP = [relfile|dbClient.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> dbClientRelToSrcP
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile dbClientRelToSrcP

    tmplData =
      if isJust maybeAuth
        then
          object
            [ "isAuthEnabled" .= True,
              "userEntityUpper" .= (AS.refName (AS.App.Auth.userEntity $ fromJust maybeAuth) :: String)
            ]
        else object []

genServerJs :: AppSpec -> Generator FileDraft
genServerJs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/server.js|])
      (C.asServerFile [relfile|src/server.js|])
      ( Just $
          object
            [ "doesServerSetupFnExist" .= isJust maybeSetupJsFunction,
              "serverSetupJsFnImportStatement" .= fromMaybe "" maybeSetupJsFnImportStmt,
              "serverSetupJsFnIdentifier" .= fromMaybe "" maybeSetupJsFnImportIdentifier,
              "isPgBossJobExecutorUsed" .= isPgBossJobExecutorUsed spec
            ]
      )
  where
    maybeSetupJsFunction = AS.App.Server.setupFn =<< AS.App.server (snd $ getApp spec)
    maybeSetupJsFnImportDetails = getJsImportDetailsForExtFnImport extServerCodeDirInServerSrcDirP <$> maybeSetupJsFunction
    (maybeSetupJsFnImportIdentifier, maybeSetupJsFnImportStmt) =
      (fst <$> maybeSetupJsFnImportDetails, snd <$> maybeSetupJsFnImportDetails)

extServerCodeDirInServerSrcDirP :: Path Posix (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extServerCodeDirInServerSrcDirP = fromJust $ relDirToPosix extServerCodeDirInServerSrcDir

genRoutesDir :: AppSpec -> Generator [FileDraft]
genRoutesDir spec =
  -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
  -- but I did not bother with it yet since it is used only here for now.
  return
    [ C.mkTmplFdWithDstAndData
        (C.asTmplFile [relfile|src/routes/index.js|])
        (C.asServerFile [relfile|src/routes/index.js|])
        ( Just $
            object
              [ "operationsRouteInRootRouter" .= (operationsRouteInRootRouter :: String),
                "isAuthEnabled" .= (isAuthEnabled spec :: Bool)
              ]
        )
    ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

depsRequiredByPassport :: AppSpec -> [App.Dependency.Dependency]
depsRequiredByPassport spec =
  AS.Dependency.fromList $
    concat
      [ [("passport", "0.6.0") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True],
        [("passport-google-oauth20", "2.0.0") | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True]
      ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

areServerPatchesUsed :: AppSpec -> Generator Bool
areServerPatchesUsed spec = not . null <$> genPatches spec

genPatches :: AppSpec -> Generator [FileDraft]
genPatches spec = patchesRequiredByPassport spec

patchesRequiredByPassport :: AppSpec -> Generator [FileDraft]
patchesRequiredByPassport spec =
  return $
    [ C.mkTmplFd (C.asTmplFile [relfile|patches/oauth+0.9.15.patch|])
      | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- Allows us to make specific changes to dependencies of our dependencies.
-- This is helpful if something broke in later versions, etc.
-- Ref: https://docs.npmjs.com/cli/v8/configuring-npm/package-json#overrides
getPackageJsonOverrides :: [Aeson.Value]
getPackageJsonOverrides = map buildOverrideData (designateLastElement overrides)
  where
    overrides :: [(String, String, String)]
    overrides =
      [ -- sodium-native > 3.3.0 broke deploying on Heroku.
        -- Ref: https://github.com/sodium-friends/sodium-native/issues/160
        ("secure-password", "sodium-native", "3.3.0")
      ]

    -- NOTE: We must designate the last element so the JSON template can omit the final comma.
    buildOverrideData :: (String, String, String, Bool) -> Aeson.Value
    buildOverrideData (packageName, dependencyName, dependencyVersion, lastElement) =
      object
        [ "packageName" .= packageName,
          "dependencyName" .= dependencyName,
          "dependencyVersion" .= dependencyVersion,
          "last" .= lastElement
        ]

    designateLastElement :: [(String, String, String)] -> [(String, String, String, Bool)]
    designateLastElement [] = []
    designateLastElement l =
      map (\(x1, x2, x3) -> (x1, x2, x3, False)) (init l)
        ++ map (\(x1, x2, x3) -> (x1, x2, x3, True)) [last l]
