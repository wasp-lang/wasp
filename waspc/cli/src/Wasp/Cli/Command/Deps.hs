module Wasp.Cli.Command.Deps
  ( deps,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (defaultCompileOptions)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspNpmPackageAvailable (WaspNpmPackageAvailable (WaspNpmPackageAvailable), brokenInstallError, notInstalledError)
import Wasp.Docs (versionedWaspDocsUrl)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalFeatureNpmDeps (getNpmDepsOfUsedOptionalFeatures)
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project (analyzeWaspProject)
import Wasp.Project.WaspNpmPackage (InstalledWaspNpmPackageError (..), tryGettingInstalledWaspNpmPackageRequiredPeerDeps)
import qualified Wasp.Util.TextStyle as TextStyle

deps :: Command ()
deps = do
  InWaspProject waspProjectDir <- require
  WaspNpmPackageAvailable <- require
  (appSpecOrAnalyzerErrors, _analyzerWarnings) <-
    liftIO $ analyzeWaspProject waspProjectDir (defaultCompileOptions waspProjectDir)
  appSpec <-
    either
      (throwError . CommandError "Determining dependencies failed due to a compilation error in your Wasp project" . unwords)
      return
      appSpecOrAnalyzerErrors

  waspNpmPackageRequiredPeerDeps <-
    liftIO (tryGettingInstalledWaspNpmPackageRequiredPeerDeps waspProjectDir) >>= \case
      Right peerDeps -> return peerDeps
      Left WaspNpmPackageNotInstalled -> throwError notInstalledError
      Left (BrokenWaspNpmPackage reason) -> throwError $ brokenInstallError reason

  liftIO $ putStrLn $ depsMessage appSpec waspNpmPackageRequiredPeerDeps

depsMessage :: AppSpec -> [Npm.Dependency.Dependency] -> String
depsMessage appSpec waspNpmPackageRequiredPeerDeps =
  unlines $
    [ "",
      TextStyle.styleTitle "Below are the dependencies Wasp uses in your project.",
      "To use another version, see " ++ versionedWaspDocsUrl "advanced/dependencies#overriding-wasps-dependencies-advanced",
      ""
    ]
      ++ printDeps
        ("Peer dependencies of " ++ waspNpmPackageName ++ ". You can import them in your code as if you specified them yourself:")
        waspNpmPackageRequiredPeerDeps
      ++ [""]
      ++ printDeps
        "Packages for the features your app uses, which your project specifies:"
        (getNpmDepsOfUsedOptionalFeatures appSpec)

printDeps :: String -> [Npm.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  TextStyle.styleTitle dependenciesTitle : map printDep dependencies

printDep :: Npm.Dependency.Dependency -> String
printDep dep =
  TextStyle.applyStyles [TextStyle.Cyan] (Npm.Dependency.name dep)
    ++ "@"
    ++ TextStyle.applyStyles [TextStyle.Yellow] (Npm.Dependency.version dep)
