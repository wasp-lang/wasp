module Generator.ServerGenerator
    ( genServer
    , operationsRouteInRootRouter
    ) where

import           Data.Aeson                                      (object, (.=))
import           Data.List                                       (intercalate)
import           Data.Maybe                                      (fromJust,
                                                                  isJust)
import qualified Path                                            as P
import           StrongPath                                      ((</>))
import qualified StrongPath                                      as SP

import           CompileOptions                                  (CompileOptions)
import           Generator.Common                                (nodeVersionAsText)
import           Generator.ExternalCodeGenerator                 (generateExternalCodeDir)
import           Generator.FileDraft                             (FileDraft, createCopyFileDraft)
import           Generator.PackageJsonGenerator                  (resolveNpmDeps,
                                                                  toPackageJsonDependenciesString)
import           Generator.ServerGenerator.AuthG                 (genAuth)
import           Generator.ServerGenerator.Common                (asServerFile,
                                                                  asTmplFile)
import qualified Generator.ServerGenerator.Common                as C
import           Generator.ServerGenerator.ConfigG               (genConfigFile)
import qualified Generator.ServerGenerator.ExternalCodeGenerator as ServerExternalCodeGenerator
import           Generator.ServerGenerator.OperationsG           (genOperations)
import           Generator.ServerGenerator.OperationsRoutesG     (genOperationsRoutes)
import qualified NpmDependency                                   as ND
import           Wasp                                            (Wasp, getAuth)
import qualified Wasp
import qualified Wasp.Auth
import qualified Wasp.NpmDependencies                            as WND


genServer :: Wasp -> CompileOptions -> [FileDraft]
genServer wasp _ = concat
    [ [genReadme wasp]
    , [genPackageJson wasp waspNpmDeps]
    , [genNpmrc wasp]
    , [genNvmrc wasp]
    , [genGitignore wasp]
    , genSrcDir wasp
    , generateExternalCodeDir ServerExternalCodeGenerator.generatorStrategy wasp
    , genDotEnv wasp
    ]

genDotEnv :: Wasp -> [FileDraft]
genDotEnv wasp =
    case Wasp.getDotEnvFile wasp of
        Just srcFilePath ->
            [ createCopyFileDraft
              (C.serverRootDirInProjectRootDir </> SP.fromPathRelFile [P.relfile|.env|])
              srcFilePath
            ]
        Nothing -> []

genReadme :: Wasp -> FileDraft
genReadme _ = C.copyTmplAsIs (asTmplFile [P.relfile|README.md|])

genPackageJson :: Wasp -> [ND.NpmDependency] -> FileDraft
genPackageJson wasp waspDeps = C.makeTemplateFD
    (asTmplFile [P.relfile|package.json|])
    (asServerFile [P.relfile|package.json|])
    (Just $ object
     [ "wasp" .= wasp
     , "depsChunk" .= toPackageJsonDependenciesString (resolvedWaspDeps ++ resolvedUserDeps)
     , "nodeVersion" .= nodeVersionAsText
     , "startProductionScript" .= concat
         [ if not (null $ Wasp.getPSLEntities wasp) then "npm run db-migrate && " else ""
         , "NODE_ENV=production node ./src/server.js"
         ]
     ])
  where
    (resolvedWaspDeps, resolvedUserDeps) =
        case resolveNpmDeps waspDeps userDeps of
            Right deps -> deps
            Left depsAndErrors -> error $ intercalate " ; " $ map snd depsAndErrors

    userDeps :: [ND.NpmDependency]
    userDeps = WND._dependencies $ Wasp.getNpmDependencies wasp

waspNpmDeps :: [ND.NpmDependency]
waspNpmDeps = ND.fromList
    [ ("cookie-parser", "~1.4.4")
    , ("cors", "^2.8.5")
    , ("debug", "~2.6.9")
    , ("express", "~4.16.1")
    , ("morgan", "~1.9.1")
    , ("@prisma/client", "2.12.1")
    , ("jsonwebtoken", "^8.5.1")
    , ("secure-password", "^4.0.0")
    , ("dotenv", "8.2.0")
    ]

-- TODO: Also extract devDependencies like we did dependencies (waspNpmDeps).

genNpmrc :: Wasp -> FileDraft
genNpmrc _ = C.makeTemplateFD (asTmplFile [P.relfile|npmrc|])
                              (asServerFile [P.relfile|.npmrc|])
                              Nothing

genNvmrc :: Wasp -> FileDraft
genNvmrc _ = C.makeTemplateFD (asTmplFile [P.relfile|nvmrc|])
                              (asServerFile [P.relfile|.nvmrc|])
                              (Just (object ["nodeVersion" .= ('v' : nodeVersionAsText)]))

genGitignore :: Wasp -> FileDraft
genGitignore _ = C.makeTemplateFD (asTmplFile [P.relfile|gitignore|])
                                  (asServerFile [P.relfile|.gitignore|])
                                  Nothing

genSrcDir :: Wasp -> [FileDraft]
genSrcDir wasp = concat
    [ [C.copySrcTmplAsIs $ C.asTmplSrcFile [P.relfile|app.js|]]
    , [C.copySrcTmplAsIs $ C.asTmplSrcFile [P.relfile|server.js|]]
    , [C.copySrcTmplAsIs $ C.asTmplSrcFile [P.relfile|utils.js|]]
    , [C.copySrcTmplAsIs $ C.asTmplSrcFile [P.relfile|core/HttpError.js|]]
    , [genDbClient wasp]
    , [genConfigFile wasp]
    , genRoutesDir wasp
    , genOperationsRoutes wasp
    , genOperations wasp
    , genAuth wasp
    ]

genDbClient :: Wasp -> FileDraft
genDbClient wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
    where
        maybeAuth = getAuth wasp

        dbClientRelToSrcP = [P.relfile|dbClient.js|]
        tmplFile = C.asTmplFile $ [P.reldir|src|] P.</> dbClientRelToSrcP
        dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile dbClientRelToSrcP

        tmplData =
            if isJust maybeAuth
                then object
                    [ "isAuthEnabled" .= True
                    , "userEntityUpper" .= Wasp.Auth._userEntity (fromJust maybeAuth)
                    ]
                else object []

genRoutesDir :: Wasp -> [FileDraft]
genRoutesDir wasp =
    -- TODO(martin): We will probably want to extract "routes" path here same as we did with "src", to avoid hardcoding,
    -- but I did not bother with it yet since it is used only here for now.
    [ C.makeTemplateFD
        (asTmplFile [P.relfile|src/routes/index.js|])
        (asServerFile [P.relfile|src/routes/index.js|])
        (Just $ object
            [ "operationsRouteInRootRouter" .= operationsRouteInRootRouter
            , "isAuthEnabled" .= isJust (getAuth wasp)
            ]
        )
    ]

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"
