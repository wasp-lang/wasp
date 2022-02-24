module GoldenTest
  ( runGoldenTest,
    GoldenTest (..),
  )
where

import Common (getTestOutputsDir)
import Control.Monad (filterM)
import Data.List (sort)
import Data.Text (pack, replace, unpack)
import ShellCommands (MakeShellCommand, ShellCommandContext (..))
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (makeRelative, takeFileName)
import qualified System.FilePath as FP
import System.Process (callCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

data GoldenTest = GoldenTest {_goldenTestName :: String, _makeShellCommand :: MakeShellCommand}

-- | This runs a golden test by creating a Wasp project (via `wasp-cli new`), running commands,
-- and then comparing all file outputs to the corresponding golden test output directory.
runGoldenTest :: GoldenTest -> IO TestTree
runGoldenTest goldenTest = do
  testOutputsDirAbsSp <- getTestOutputsDir
  let testOutputsDirAbsFp = SP.fromAbsDir testOutputsDirAbsSp

  let context =
        ShellCommandContext
          { _ctxtCurrentProjectName = _goldenTestName goldenTest,
            _ctxtCurrentOutputDirAbsFp = testOutputsDirAbsFp FP.</> (_goldenTestName goldenTest ++ "-current"),
            _ctxtGoldenOutputDirAbsFp = testOutputsDirAbsFp FP.</> (_goldenTestName goldenTest ++ "-golden")
          }

  -- Remove existing current output files from a prior test run.
  callCommand $ "rm -rf " ++ _ctxtCurrentOutputDirAbsFp context

  -- Create current output dir as well as the golden output dir, if missing.
  callCommand $ "mkdir " ++ _ctxtCurrentOutputDirAbsFp context
  callCommand $ "mkdir -p " ++ _ctxtGoldenOutputDirAbsFp context

  -- Provide the context to the command so it can generate the correct Wasp CLI commands and paths.
  let shellCommand = _makeShellCommand goldenTest context
  putStrLn $ "Running the following command: " ++ shellCommand

  -- Run the series of commands within the context of a current output dir.
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ "cd " ++ _ctxtCurrentOutputDirAbsFp context ++ " && " ++ shellCommand

  currentOutputAbsFps <- listRelevantTestOutputFiles $ _ctxtCurrentOutputDirAbsFp context
  let manifestAbsFp = _ctxtCurrentOutputDirAbsFp context FP.</> "files.manifest"

  writeFileManifest (_ctxtCurrentOutputDirAbsFp context) currentOutputAbsFps manifestAbsFp

  let remapCurrentPathToGolden fp = unpack $ replace (pack $ _ctxtCurrentOutputDirAbsFp context) (pack $ _ctxtGoldenOutputDirAbsFp context) (pack fp)

  return $
    testGroup
      (_goldenTestName goldenTest)
      [ goldenVsFileDiff
          currentOutputAbsFp -- The test name that shows in the output.
          (\ref new -> ["diff", "-u", ref, new])
          goldenOutputAbsFp
          currentOutputAbsFp
          (return ()) -- A no-op command that normally generates the file under test, but we did that in bulk above.
        | currentOutputAbsFp <- manifestAbsFp : currentOutputAbsFps,
          let goldenOutputAbsFp = remapCurrentPathToGolden currentOutputAbsFp
      ]
  where
    listRelevantTestOutputFiles :: FilePath -> IO [FilePath]
    listRelevantTestOutputFiles dirToFilterAbsFp =
      getDirFiltered (return <$> isTestOutputFileTestable) dirToFilterAbsFp >>= filterM doesFileExist

    isTestOutputFileTestable :: FilePath -> Bool
    isTestOutputFileTestable fp =
      -- TODO: Ideally we would not ignore `package.json`, but in CI the order of dependencies differs. :/
      --       Come back and check on this again after rebasing main, and if still causing failures, create an Issue.
      takeFileName fp `notElem` [".waspinfo", ".gitignore", "node_modules", "dev.db", "dev.db-journal", "package.json", "package-lock.json", "golden.manifest"]

    writeFileManifest :: [Char] -> [FilePath] -> FilePath -> IO ()
    writeFileManifest baseAbsFp filePaths manifestAbsFp = do
      let sortedRelativeFilePaths = unlines . sort . map (makeRelative baseAbsFp) $ filePaths
      writeFile manifestAbsFp sortedRelativeFilePaths
