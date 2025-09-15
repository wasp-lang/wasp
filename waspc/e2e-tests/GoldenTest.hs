module GoldenTest
  ( runGoldenTest,
    makeGoldenTest,
    GoldenTest,
  )
where

import Common
  ( SnapshotType (..),
    expectedFilesManifestFileInSnapshotDir,
    getTestOutputsDir,
    snapshotDirInTestOutputsDir,
  )
import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromJust)
import Data.Text (pack, replace, unpack)
import ShellCommands
  ( GoldenTestContext (..),
    ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    combineShellCommands,
  )
import qualified StrongPath as SP
import qualified StrongPath as Sp
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (makeRelative, takeFileName)
import qualified System.FilePath as FP
import System.Process (callCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

data GoldenTest = GoldenTest
  { _goldenTestName :: String,
    _goldenTestCommands :: ShellCommandBuilder GoldenTestContext [ShellCommand]
  }

makeGoldenTest :: String -> ShellCommandBuilder GoldenTestContext [ShellCommand] -> GoldenTest
makeGoldenTest name commands = GoldenTest {_goldenTestName = name, _goldenTestCommands = commands}

-- | This runs a golden test by creating a Wasp project (via `wasp-cli new`), running commands,
-- and then comparing all file outputs to the corresponding golden test output directory.
runGoldenTest :: GoldenTest -> IO TestTree
runGoldenTest goldenTest = do
  let goldenTestName = _goldenTestName goldenTest
  testOutputsDir <- getTestOutputsDir

  let currentSnapshotAbsDir = testOutputsDir SP.</> snapshotDirInTestOutputsDir goldenTestName Current
  let goldenSnapshotAbsDir = testOutputsDir SP.</> snapshotDirInTestOutputsDir goldenTestName Golden
  let expectedFilesManigestAbsFile = currentSnapshotAbsDir SP.</> expectedFilesManifestFileInSnapshotDir

  let currentSnapshotDirAbsFp = SP.fromAbsDir currentSnapshotAbsDir
  let goldenSnapshotDirAbsFp = SP.fromAbsDir goldenSnapshotAbsDir
  let expectedFilesManifestFileAbsFp = Sp.fromAbsFile expectedFilesManigestAbsFile

  -- Remove existing current output files from a prior test run.
  callCommand $ "rm -rf " ++ currentSnapshotDirAbsFp

  -- Create current output dir as well as the golden output dir, if missing.
  callCommand $ "mkdir " ++ currentSnapshotDirAbsFp
  callCommand $ "mkdir -p " ++ goldenSnapshotDirAbsFp

  let context = GoldenTestContext {_goldenTestProjectName = goldenTestName}
  let shellCommand = combineShellCommands $ buildShellCommand context (_goldenTestCommands goldenTest)
  putStrLn $ "Running the following command: " ++ shellCommand

  -- Run the series of commands within the context of a current output dir.
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ "cd " ++ currentSnapshotDirAbsFp ++ " && " ++ shellCommand

  filesForCheckingExistenceAbsFps <- getFilesForCheckingExistence currentSnapshotDirAbsFp
  filesForCheckingContentAbsFps <- (expectedFilesManifestFileAbsFp :) <$> getFilesForCheckingContent currentSnapshotDirAbsFp

  writeExpectedFilesList currentSnapshotDirAbsFp filesForCheckingExistenceAbsFps expectedFilesManifestFileAbsFp
  reformatPackageJsonFiles filesForCheckingContentAbsFps

  let remapCurrentToGoldenFilePath fp = unpack $ replace (pack currentSnapshotDirAbsFp) (pack goldenSnapshotDirAbsFp) (pack fp)

  return $
    testGroup
      goldenTestName
      [ goldenVsFileDiff
          currentSnapshotAbsFp -- The test name.
          (\ref new -> ["diff", "-u", ref, new])
          goldenSnapshotAbsFp
          currentSnapshotAbsFp
          (return ()) -- A no-op command that normally generates the file under test, but we did that in bulk above.
        | currentSnapshotAbsFp <- filesForCheckingContentAbsFps,
          let goldenSnapshotAbsFp = remapCurrentToGoldenFilePath currentSnapshotAbsFp
      ]
  where
    getFilesForCheckingExistence :: FilePath -> IO [FilePath]
    getFilesForCheckingExistence dirToFilterAbsFp =
      getDirFiltered (return <$> shouldCheckFileExistence) dirToFilterAbsFp >>= filterM doesFileExist

    getFilesForCheckingContent :: FilePath -> IO [FilePath]
    getFilesForCheckingContent dirToFilterAbsFp =
      getDirFiltered (return <$> shouldCheckFileContents) dirToFilterAbsFp >>= filterM doesFileExist

    shouldCheckFileExistence :: FilePath -> Bool
    shouldCheckFileExistence filePath =
      takeFileName filePath
        `notElem` [ ".DS_Store",
                    "node_modules"
                  ]

    shouldCheckFileContents :: FilePath -> Bool
    shouldCheckFileContents filePath =
      takeFileName filePath
        `notElem` [ ".DS_Store",
                    "dev.db",
                    "dev.db-journal",
                    ".gitignore",
                    ".waspinfo",
                    "package-lock.json",
                    "node_modules",
                    "tsconfig.tsbuildinfo",
                    "dist"
                  ]

    writeExpectedFilesList :: String -> [FilePath] -> FilePath -> IO ()
    writeExpectedFilesList baseAbsFp filesForCheckingExistenceAbsFps expectedFilesListAbsFp = do
      let sortedRelativeFilePaths = unlines . sort . map (makeRelative baseAbsFp) $ filesForCheckingExistenceAbsFps
      writeFile expectedFilesListAbsFp sortedRelativeFilePaths

    -- While Wasp deterministically produces package.json files in the generated code,
    -- later calls to `npm install` can reformat them (e.g. it sorts dependencies).
    -- Also, different versions of npm may produce different (but semantically equivalent) package.json files.
    -- All of this can result in snapshot tests flagging these files as being different when it should not.
    -- Ref: https://github.com/wasp-lang/wasp/issues/482
    reformatPackageJsonFiles :: [FilePath] -> IO ()
    reformatPackageJsonFiles filePaths = do
      let packageJsonFilePaths = filter isPathToPackageJson filePaths
      mapM_ reformatJson packageJsonFilePaths
      where
        isPathToPackageJson :: FilePath -> Bool
        isPathToPackageJson = ((FP.pathSeparator : "package.json") `isSuffixOf`)

        aesonPrettyConfig :: AesonPretty.Config
        aesonPrettyConfig =
          AesonPretty.Config
            { AesonPretty.confIndent = AesonPretty.Spaces 2,
              AesonPretty.confCompare = compare,
              AesonPretty.confNumFormat = AesonPretty.Generic,
              AesonPretty.confTrailingNewline = True
            }

        reformatJson :: FilePath -> IO ()
        reformatJson jsonFilePath =
          BSL.writeFile jsonFilePath . AesonPretty.encodePretty' aesonPrettyConfig . unsafeDecodeAnyJson
            =<< B.readFile jsonFilePath
          where
            unsafeDecodeAnyJson :: B.ByteString -> Aeson.Value
            unsafeDecodeAnyJson = fromJust . Aeson.decodeStrict
