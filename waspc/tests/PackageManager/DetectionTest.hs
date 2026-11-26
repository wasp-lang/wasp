module PackageManager.DetectionTest where

import Control.Exception (bracket)
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
      detectInvokingPackageManager `shouldReturn` Detection Bun UserAgent

      withRepositoryRoot $ \repositoryRoot ->
        detectProjectPackageManager repositoryRoot `shouldReturn` Detection Bun UserAgent

      withRepositoryRoot $ \repositoryRoot -> do
        let lockfile = repositoryRoot </> [relfile|pnpm-lock.yaml|]
        writeFile (SP.fromAbsFile lockfile) ""
        detectProjectPackageManager repositoryRoot
          `shouldReturn` Detection Pnpm (LockfileOrWorkspaceFile lockfile)

    withUserAgent Nothing $ do
      detectInvokingPackageManager `shouldReturn` Detection Npm Default

      withRepositoryRoot $ \repositoryRoot ->
        detectProjectPackageManager repositoryRoot `shouldReturn` Detection Npm Default
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
        writeFile (SP.fromAbsFile $ repositoryRoot </> [relfile|.git|]) ""
        check (repositoryRoot :: SP.Path' SP.Abs SP.Dir')

    userAgentEnvVarName = "npm_config_user_agent"
