module Node.BinTest where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, exeExtension, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Node.Bin (findNpmBin)

spec_NodeBin :: Spec
spec_NodeBin =
  describe "findNpmBin" $ do
    it "finds the executable in the closest node_modules/.bin directory" $
      withProjectDirs $ \projectDir packageDir -> do
        _ <- createNodeBin projectDir "tool"
        packageTool <- createNodeBin packageDir "tool"
        findNpmBin (toAbsDir packageDir) "tool" `shouldReturn` Just packageTool

    it "looks for the executable in the ancestors' node_modules/.bin directories" $
      withProjectDirs $ \projectDir packageDir -> do
        projectTool <- createNodeBin projectDir "tool"
        findNpmBin (toAbsDir packageDir) "tool" `shouldReturn` Just projectTool

    it "returns Nothing if the executable isn't in any node_modules/.bin directory" $
      withProjectDirs $ \_ packageDir ->
        findNpmBin (toAbsDir packageDir) "missing-tool" `shouldReturn` Nothing

-- | Gives the test a project directory and a package directory inside it.
withProjectDirs :: (FilePath -> FilePath -> IO a) -> IO a
withProjectDirs test =
  withSystemTempDirectory "wasp-node-bin-test" $ \tempDir -> do
    let projectDir = tempDir </> "project"
        packageDir = projectDir </> "package"
    createDirectoryIfMissing True packageDir
    test projectDir packageDir

-- | Creates an executable in the directory's node_modules/.bin, like npm does
-- when installing a package with a bin, and returns its path.
createNodeBin :: FilePath -> String -> IO FilePath
createNodeBin dir binName = do
  let binDir = dir </> "node_modules" </> ".bin"
      binPath = binDir </> binName <.> exeExtension
  createDirectoryIfMissing True binDir
  writeFile binPath "#!/bin/sh\n"
  getPermissions binPath >>= setPermissions binPath . setOwnerExecutable True
  return binPath

toAbsDir :: FilePath -> SP.Path' SP.Abs (SP.Dir ())
toAbsDir = fromJust . SP.parseAbsDir
