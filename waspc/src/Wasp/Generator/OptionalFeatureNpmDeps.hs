module Wasp.Generator.OptionalFeatureNpmDeps
  ( getUsedOptionalFeaturesWithNpmDeps,
    getNpmDepsOfUsedOptionalFeatures,
    allOptionalFeaturesNpmDeps,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalFeatureNpmDeps.Common (OptionalFeatureNpmDeps (..), getAllNpmDepsOfOptionalFeature)
import Wasp.Generator.SdkGenerator.EmailSender.Providers (emailSendersNpmDeps)
import Wasp.Generator.SdkGenerator.Server.AuthG (authNpmDeps)
import Wasp.Generator.SdkGenerator.Server.JobGenerator (jobsNpmDeps)
import Wasp.Generator.SdkGenerator.Server.OAuthG (socialAuthNpmDeps)
import Wasp.Generator.WebSocket (webSocketsNpmDeps)

getUsedOptionalFeaturesWithNpmDeps :: AppSpec -> [OptionalFeatureNpmDeps]
getUsedOptionalFeaturesWithNpmDeps spec = filter (`isFeatureUsed` spec) optionalFeaturesWithNpmDeps

getNpmDepsOfUsedOptionalFeatures :: AppSpec -> [Npm.Dependency.Dependency]
getNpmDepsOfUsedOptionalFeatures = concatMap getAllNpmDepsOfOptionalFeature . getUsedOptionalFeaturesWithNpmDeps

-- | The optional peer dependencies of \@wasp/wasp. npm doesn't install them on
-- its own, so a project lists the ones its app's features need, and npm checks
-- their versions. A test keeps this list in sync with
-- @packages/wasp/package.json@.
allOptionalFeaturesNpmDeps :: [Npm.Dependency.Dependency]
allOptionalFeaturesNpmDeps = concatMap getAllNpmDepsOfOptionalFeature optionalFeaturesWithNpmDeps

optionalFeaturesWithNpmDeps :: [OptionalFeatureNpmDeps]
optionalFeaturesWithNpmDeps = filter hasNpmDeps allOptionalFeatures
  where
    hasNpmDeps = not . null . getAllNpmDepsOfOptionalFeature

allOptionalFeatures :: [OptionalFeatureNpmDeps]
allOptionalFeatures =
  [ authNpmDeps,
    socialAuthNpmDeps,
    jobsNpmDeps,
    webSocketsNpmDeps
  ]
    ++ emailSendersNpmDeps
