module Node.BinTest where

import Data.Maybe (fromJust, fromMaybe)
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, exeExtension, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath (splitSearchPath, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P
import Test.Hspec
import Wasp.Node.Bin (nodeBinProc)

spec_NodeBin :: Spec
spec_NodeBin =
  describe "nodeBinProc" $ do
    it "runs the executable from the closest node_modules/.bin directory" $
      withProjectDirs $ \projectDir packageDir -> do
        _ <- createNodeBin projectDir "tool"
        packageTool <- createNodeBin packageDir "tool"
        process <- nodeBinProc [] (toAbsDir packageDir) "tool" ["arg"]
        P.cmdspec process `shouldBe` P.RawCommand packageTool ["arg"]

    it "looks for the executable in the ancestors' node_modules/.bin directories" $
      withProjectDirs $ \projectDir packageDir -> do
        projectTool <- createNodeBin projectDir "tool"
        process <- nodeBinProc [] (toAbsDir packageDir) "tool" []
        P.cmdspec process `shouldBe` P.RawCommand projectTool []

    it "leaves the executable to be found in PATH if it isn't in any node_modules/.bin directory" $
      withProjectDirs $ \_ packageDir -> do
        process <- nodeBinProc [] (toAbsDir packageDir) "missing-tool" []
        P.cmdspec process `shouldBe` P.RawCommand "missing-tool" []

    it "runs the process from the given directory, with the node_modules/.bin directories in PATH and the given env vars" $
      withProjectDirs $ \projectDir packageDir -> do
        process <- nodeBinProc [("SOME_VAR", "value")] (toAbsDir packageDir) "tool" []
        let envVars = fromMaybe [] $ P.env process
        P.cwd process `shouldBe` Just (SP.fromAbsDir $ toAbsDir packageDir)
        lookup "SOME_VAR" envVars `shouldBe` Just "value"
        take 2 . splitSearchPath <$> lookup "PATH" envVars
          `shouldBe` Just [packageDir </> "node_modules" </> ".bin", projectDir </> "node_modules" </> ".bin"]

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
