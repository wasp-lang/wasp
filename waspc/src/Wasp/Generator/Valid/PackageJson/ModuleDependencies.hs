module Wasp.Generator.Valid.PackageJson.ModuleDependencies
  ( moduleDependenciesValidator,
    allWaspProvidedDeps,
    waspProvidedPackageNames,
  )
where

import qualified Data.Map as Map
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.SdkGenerator (baseSdkDeps, depsRequiredForTesting, sdkDevDeps)
import Wasp.Generator.SdkGenerator.EmailSender.Providers (allEmailProviderDeps)
import Wasp.Generator.SdkGenerator.EnvValidation (depsRequiredByEnvValidation)
import Wasp.Generator.SdkGenerator.Server.JobGenerator (pgBossDependency)
import Wasp.Generator.SdkGenerator.Server.OAuthG (oauthDeps)
import Wasp.Generator.ServerGenerator (baseServerDeps)
import Wasp.Generator.ServerGenerator.AuthG (authDeps)
import qualified Wasp.Generator.Valid.PackageJson.Dependencies as Deps
import qualified Wasp.Generator.Valid.Validator as V
import qualified Wasp.Generator.WebSocket as WS

allWaspProvidedDeps :: [D.Dependency]
allWaspProvidedDeps =
  baseSdkDeps
    ++ sdkDevDeps
    ++ depsRequiredForTesting
    ++ depsRequiredByEnvValidation
    ++ baseServerDeps
    ++ authDeps
    ++ oauthDeps
    ++ [pgBossDependency]
    ++ WS.sdkDepsRequiredForWebSockets
    ++ WS.serverDepsRequiredForWebSockets
    ++ allEmailProviderDeps

waspProvidedPackageNames :: [P.PackageName]
waspProvidedPackageNames = Map.keys waspProvidedVersions

waspProvidedVersions :: Map.Map P.PackageName P.PackageVersion
waspProvidedVersions = Map.fromList $ map D.toPair allWaspProvidedDeps

moduleDependenciesValidator :: V.Validator P.PackageJson
moduleDependenciesValidator =
  V.all
    [ depValidator pkgName expectedVersion
      | (pkgName, expectedVersion) <- Map.toList waspProvidedVersions,
        depValidator <- [forbiddenInDeps, devDepVersionMustMatch]
    ]

forbiddenInDeps :: P.PackageName -> P.PackageVersion -> V.Validator P.PackageJson
forbiddenInDeps pkgName _expectedVersion =
  Deps.inDependency Deps.Runtime pkgName validator
  where
    validator :: V.Validator (Maybe P.PackageVersion)
    validator Nothing = V.success
    validator _ =
      V.failure $
        "Package "
          ++ show pkgName
          ++ " is provided by the Wasp SDK and should not be in dependencies."
          ++ " Move it to peerDependencies instead."

devDepVersionMustMatch :: P.PackageName -> P.PackageVersion -> V.Validator P.PackageJson
devDepVersionMustMatch pkgName expectedVersion =
  Deps.inDependency Deps.Development pkgName validator
  where
    validator :: V.Validator (Maybe P.PackageVersion)
    validator Nothing = V.success
    validator (Just actualVersion)
      | actualVersion == expectedVersion = V.success
      | otherwise =
          V.failure $
            "Version "
              ++ show actualVersion
              ++ " doesn't match the Wasp-provided version "
              ++ show expectedVersion
              ++ ". Update it to match."
