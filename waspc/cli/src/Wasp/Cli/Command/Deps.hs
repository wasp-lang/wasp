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
import Wasp.Cli.Command.Require.WaspNpmPackageAvailable (WaspNpmPackageAvailable (WaspNpmPackageAvailable))
import Wasp.Cli.Terminal (title)
import Wasp.Docs (versionedWaspDocsUrl)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalNpmDeps (allOptionalNpmDeps, getAllDependencies, getNpmDepsOfUsedFeatures)
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project (analyzeWaspProject)
import Wasp.Project.WaspNpmPackage (InstalledWaspNpmPackageError (..), tryGettingInstalledWaspNpmPackagePeerDeps)
import qualified Wasp.Util.Terminal as Term

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

  waspNpmPackagePeerDeps <-
    liftIO (tryGettingInstalledWaspNpmPackagePeerDeps waspProjectDir) >>= \case
      Right peerDeps -> return peerDeps
      Left WaspNpmPackageNotInstalled -> throwError $ peerDepsError "It is not installed."
      Left (BrokenWaspNpmPackage reason) -> throwError $ peerDepsError reason

  liftIO $ putStrLn $ depsMessage appSpec waspNpmPackagePeerDeps
  where
    peerDepsError = CommandError $ "Couldn't read the peer dependencies of " ++ waspNpmPackageName

depsMessage :: AppSpec -> [Npm.Dependency.Dependency] -> String
depsMessage appSpec waspNpmPackagePeerDeps =
  unlines $
    [ "",
      title "Below are the dependencies Wasp uses in your project. You can import them in your code as if you specified them yourself.",
      "To use another version, see " ++ versionedWaspDocsUrl "advanced/dependencies#overriding-wasps-dependencies-advanced",
      ""
    ]
      ++ printDeps
        ("Peer dependencies of " ++ waspNpmPackageName ++ ":")
        (filter (not . isOptionalDep) waspNpmPackagePeerDeps)
      ++ [""]
      ++ printDeps
        "Packages for the features your app uses:"
        (concatMap getAllDependencies $ getNpmDepsOfUsedFeatures appSpec)
  where
    -- Only the optional peer dependencies that the app's features need are in use.
    isOptionalDep dep = Npm.Dependency.name dep `elem` (Npm.Dependency.name <$> allOptionalNpmDeps)

printDeps :: String -> [Npm.Dependency.Dependency] -> [String]
printDeps dependenciesTitle dependencies =
  title dependenciesTitle : map printDep dependencies

printDep :: Npm.Dependency.Dependency -> String
printDep dep =
  Term.applyStyles [Term.Cyan] (Npm.Dependency.name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (Npm.Dependency.version dep)
