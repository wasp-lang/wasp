module PackageManager.DetectionTest where

import Control.Exception (bracket)
import Control.Monad ((>=>))
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.PackageManager

{- HLINT ignore spec_PackageManager_Detection "Avoid npm and npx" -}

spec_PackageManager_Detection :: Spec
spec_PackageManager_Detection = do
  -- One test, because tests run in parallel and the user agent is an
  -- environment variable.
  it "falls back from the project files to the user agent, and then to npm" $ do
    withUserAgent (Just "bun/1.1.38 npm/? node/v22.6.0 darwin arm64") $ do
      detectInvokingPackageManager >>= shouldUseNpmWithWarningAbout ["bun"]

      withRepositoryRoot $
        detectProjectPackageManager >=> shouldUseNpmWithWarningAbout ["bun"]

      withRepositoryRoot $ \repositoryRoot -> do
        _ <- writeEmptyFile repositoryRoot [relfile|package-lock.json|]
        detectProjectPackageManager repositoryRoot `shouldReturn` (Npm, Nothing)

    withUserAgent (Just "npm/10.9.0 node/v22.11.0 darwin arm64 workspaces/false") $
      detectInvokingPackageManager `shouldReturn` (Npm, Nothing)

    withUserAgent Nothing $ do
      detectInvokingPackageManager `shouldReturn` (Npm, Nothing)

      withRepositoryRoot $ \repositoryRoot ->
        detectProjectPackageManager repositoryRoot `shouldReturn` (Npm, Nothing)

  -- This never gets to the user agent, so it can run next to the test above.
  it "uses npm with a warning when Wasp doesn't support the project's package manager" $
    withRepositoryRoot $ \repositoryRoot -> do
      lockfile <- writeEmptyFile repositoryRoot [relfile|pnpm-lock.yaml|]
      detectProjectPackageManager repositoryRoot
        >>= shouldUseNpmWithWarningAbout ["pnpm", SP.fromAbsFile lockfile]
  where
    withUserAgent maybeUserAgent action =
      bracket
        (lookupEnv userAgentEnvVarName <* setUserAgent maybeUserAgent)
        setUserAgent
        (const action)

    setUserAgent = maybe (unsetEnv userAgentEnvVarName) (setEnv userAgentEnvVarName)

    -- The @.git@ keeps detection from looking above the temporary directory.
    withRepositoryRoot check =
      withSystemTempDirectory "wasp-package-manager" $ \tempDir -> do
        repositoryRoot <- fromJust . SP.parseAbsDir <$> canonicalizePath tempDir
        _ <- writeEmptyFile repositoryRoot [relfile|.git|]
        check (repositoryRoot :: SP.Path' SP.Abs SP.Dir')

    writeEmptyFile dir file = do
      let absFile = dir </> file
      writeFile (SP.fromAbsFile absFile) ""
      return (absFile :: SP.Path' SP.Abs SP.File')

    userAgentEnvVarName = "npm_config_user_agent"

    shouldUseNpmWithWarningAbout mentions (packageManager, warning) = do
      packageManager `shouldBe` Npm
      warning `shouldSatisfy` maybe False (\text -> all (`isInfixOf` text) mentions)
