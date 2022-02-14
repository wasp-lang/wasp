module GoldenTest
  ( GoldenTest,
    runGoldenTest,
  )
where

import Common (getGoldensDir)
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

type GoldenTest = IO TestTree

-- | This runs a golden test by creating a Wasp project, running commands,
-- and then comparing all file outputs to the corresponding golden test output directory.
runGoldenTest :: String -> MakeShellCommand -> GoldenTest
runGoldenTest goldenTestName makeShellCommand = do
  goldensDirAbsSp <- getGoldensDir
  let goldensDirAbsFp = SP.fromAbsDir goldensDirAbsSp

  let context =
        ShellCommandContext
          { currentProjectName = goldenTestName,
            currentOutputDirAbsFp = goldensDirAbsFp FP.</> (goldenTestName ++ "-current"),
            goldenOutputDirAbsFp = goldensDirAbsFp FP.</> (goldenTestName ++ "-golden")
          }

  -- Remove existing current output files from a prior test run.
  callCommand $ "rm -rf " ++ currentOutputDirAbsFp context

  -- Create current output dir as well as the golden output dir, if missing.
  callCommand $ "mkdir " ++ currentOutputDirAbsFp context
  callCommand $ "mkdir -p " ++ goldenOutputDirAbsFp context

  -- Provide the context to the command so it can generate the correct Wasp CLI commands and paths.
  let shellCommand = makeShellCommand context
  putStrLn $ "Running the following command: " ++ shellCommand

  -- Run the series of commands within the context of a current output dir.
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ "cd " ++ currentOutputDirAbsFp context ++ " && " ++ shellCommand

  currentOutputAbsFps <- getAllDirFilesRecursivelyFiltered $ currentOutputDirAbsFp context
  let manifestAbsFp = currentOutputDirAbsFp context FP.</> "files.manifest"

  writeFileManifest (currentOutputDirAbsFp context) currentOutputAbsFps manifestAbsFp

  return $
    testGroup
      goldenTestName
      [ goldenVsFileDiff
          currentOutputAbsFp -- The test name that shows in the output.
          (\ref new -> ["diff", "-u", ref, new])
          goldenOutputAbsFp
          currentOutputAbsFp
          (return ()) -- A no-op command that normally generates the file under test, but we did that in bulk above.
        | currentOutputAbsFp <- manifestAbsFp : currentOutputAbsFps,
          let goldenOutputAbsFp = unpack $ replace (pack $ currentOutputDirAbsFp context) (pack $ goldenOutputDirAbsFp context) (pack currentOutputAbsFp)
      ]
  where
    -- TODO: Ideally we would not ignore `package.json`, but in CI the order of dependencies differs. :/
    dirFilter fp =
      return $ notElem (takeFileName fp) [".waspinfo", ".gitignore", "node_modules", "dev.db", "dev.db-journal", "package.json", "package-lock.json", "golden.manifest"]
    getAllDirFilesRecursivelyFiltered dirToFilterAbsFp =
      getDirFiltered dirFilter dirToFilterAbsFp >>= filterM doesFileExist
    writeFileManifest baseAbsFp filePaths manifestAbsFp = do
      let sortedRelativeFilePaths = unlines . sort . map (makeRelative baseAbsFp) $ filePaths
      writeFile manifestAbsFp sortedRelativeFilePaths
