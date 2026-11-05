module Job.CreateProcess.SearchPathTest where

import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory (canonicalizePath, createDirectoryIfMissing, exeExtension, findExecutable, getPermissions, setOwnerExecutable, setPermissions)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P
import Test.Hspec
import Wasp.Job.CreateProcess.SearchPath (findExecutableFrom, findExecutableInSearchPath, getSearchPathEnvVars)

{- HLINT ignore spec_JobCreateProcessSearchPath "Avoid npm and npx" -}

spec_JobCreateProcessSearchPath :: Spec
spec_JobCreateProcessSearchPath = do
  describe "findExecutableFrom" $ do
    it "finds the executable in the closest node_modules/.bin directory" $
      withProjectDirs $ \projectDir packageDir -> do
        _ <- createNpmBin projectDir "tool"
        packageTool <- createNpmBin packageDir "tool"
        findExecutableFrom (toAbsDir packageDir) "tool" `shouldReturn` packageTool

    it "looks for the executable in the ancestors' node_modules/.bin directories" $
      withProjectDirs $ \projectDir packageDir -> do
        projectTool <- createNpmBin projectDir "tool"
        findExecutableFrom (toAbsDir packageDir) "tool" `shouldReturn` projectTool

    it "looks for the executable in the PATH after the node_modules/.bin directories" $
      withProjectDirs $ \_ packageDir -> do
        nodeInPath <- fromJust <$> findExecutable "node"
        findExecutableFrom (toAbsDir packageDir) "node" `shouldReturn` nodeInPath

    it "fails if the executable isn't in any node_modules/.bin directory or the PATH" $
      withProjectDirs $ \_ packageDir ->
        findExecutableFrom (toAbsDir packageDir) "missing-tool" `shouldThrow` anyIOException

  -- Wasp's working directory while testing is never the temp package directory,
  -- so these check that relative entries aren't resolved against it.
  describe "findExecutableInSearchPath" $ do
    it "resolves relative search path entries against the given directory" $
      withProjectDirs $ \_ packageDir -> do
        tool <- createExecutable (packageDir </> "bin") "tool"
        findExecutableInSearchPath (toAbsDir packageDir) "tool" ["bin"] `shouldReturn` tool

    it "resolves the current directory entry, which is what an empty PATH entry is on POSIX, to the given directory" $
      withProjectDirs $ \_ packageDir -> do
        tool <- createExecutable packageDir "tool"
        findExecutableInSearchPath (toAbsDir packageDir) "tool" ["."] `shouldReturn` tool

  describe "getSearchPathEnvVars" $ do
    it "puts the node_modules/.bin directories, closest first, before the current PATH" $
      withProjectDirs $ \projectDir packageDir -> do
        currentPath <- maybe [] splitSearchPath <$> lookupEnv "PATH"
        [(name, value)] <- getSearchPathEnvVars (toAbsDir packageDir)
        name `shouldBe` "PATH"
        let pathDirs = splitSearchPath value
        take 2 pathDirs `shouldBe` [packageDir </> "node_modules" </> ".bin", projectDir </> "node_modules" </> ".bin"]
        drop (length pathDirs - length currentPath) pathDirs `shouldBe` currentPath

    -- Checks our assumption about npm's behavior, so we notice if a future npm
    -- version stops matching it.
    it "puts the same node_modules/.bin directories in the PATH as npm does for scripts" $
      withProjectDirs $ \_ packageDir -> do
        writeFile (packageDir </> "package.json") "{\"scripts\": {\"print-path\": \"node -p process.env.PATH\"}}"
        npmPathDirs <- splitSearchPath . concat . lines <$> P.readCreateProcess ((P.proc "npm" ["run", "--silent", "print-path"]) {P.cwd = Just packageDir}) ""
        currentPath <- maybe [] splitSearchPath <$> lookupEnv "PATH"
        [(_, value)] <- getSearchPathEnvVars (toAbsDir packageDir)
        let pathDirs = splitSearchPath value
        take (length pathDirs - length currentPath) pathDirs
          `shouldBe` takeWhile (("node_modules" </> ".bin") `isSuffixOf`) npmPathDirs

-- | Gives the test a project directory and a package directory inside it.
withProjectDirs :: (FilePath -> FilePath -> IO a) -> IO a
withProjectDirs test =
  withSystemTempDirectory "wasp-node-bin-test" $ \tempDir' -> do
    -- npm resolves symlinks in the directory it runs in (e.g. macOS's /var).
    tempDir <- canonicalizePath tempDir'
    let projectDir = tempDir </> "project"
        packageDir = projectDir </> "package"
    createDirectoryIfMissing True packageDir
    test projectDir packageDir

-- | Creates an executable in the directory's node_modules/.bin, like npm does
-- when installing a package with a bin, and returns its path.
createNpmBin :: FilePath -> String -> IO FilePath
createNpmBin dir = createExecutable (dir </> "node_modules" </> ".bin")

-- | Creates an executable with the given name in the directory and returns its
-- path.
createExecutable :: FilePath -> String -> IO FilePath
createExecutable dir executableName = do
  let executablePath = dir </> executableName <.> exeExtension
  createDirectoryIfMissing True dir
  writeFile executablePath "#!/bin/sh\n"
  getPermissions executablePath >>= setPermissions executablePath . setOwnerExecutable True
  return executablePath

toAbsDir :: FilePath -> SP.Path' SP.Abs (SP.Dir ())
toAbsDir = fromJust . SP.parseAbsDir
