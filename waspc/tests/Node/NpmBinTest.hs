module Node.NpmBinTest where

import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory (canonicalizePath, createDirectoryIfMissing, exeExtension, getPermissions, setOwnerExecutable, setPermissions)
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P
import Test.Hspec
import Wasp.Node.NpmBin (findNpmBin, getNpmBinEnvVars)

{- HLINT ignore spec_NodeNpmBin "Avoid npm and npx" -}

spec_NodeNpmBin :: Spec
spec_NodeNpmBin = do
  describe "findNpmBin" $ do
    it "finds the executable in the closest node_modules/.bin directory" $
      withProjectDirs $ \projectDir packageDir -> do
        _ <- createNpmBin projectDir "tool"
        packageTool <- createNpmBin packageDir "tool"
        findNpmBin (toAbsDir packageDir) "tool" `shouldReturn` Just packageTool

    it "looks for the executable in the ancestors' node_modules/.bin directories" $
      withProjectDirs $ \projectDir packageDir -> do
        projectTool <- createNpmBin projectDir "tool"
        findNpmBin (toAbsDir packageDir) "tool" `shouldReturn` Just projectTool

    it "returns Nothing if the executable isn't in any node_modules/.bin directory" $
      withProjectDirs $ \_ packageDir ->
        findNpmBin (toAbsDir packageDir) "missing-tool" `shouldReturn` Nothing

  describe "getNpmBinEnvVars" $ do
    it "puts the node_modules/.bin directories, closest first, before the current PATH" $
      withProjectDirs $ \projectDir packageDir -> do
        currentPath <- maybe [] splitSearchPath <$> lookupEnv "PATH"
        [(name, value)] <- getNpmBinEnvVars (toAbsDir packageDir)
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
        [(_, value)] <- getNpmBinEnvVars (toAbsDir packageDir)
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
createNpmBin dir binName = do
  let binDir = dir </> "node_modules" </> ".bin"
      binPath = binDir </> binName <.> exeExtension
  createDirectoryIfMissing True binDir
  writeFile binPath "#!/bin/sh\n"
  getPermissions binPath >>= setPermissions binPath . setOwnerExecutable True
  return binPath

toAbsDir :: FilePath -> SP.Path' SP.Abs (SP.Dir ())
toAbsDir = fromJust . SP.parseAbsDir
