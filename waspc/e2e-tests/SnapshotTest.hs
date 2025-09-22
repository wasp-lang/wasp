module SnapshotTest
  ( SnapshotTest,
    makeSnapshotTest,
    runSnapshotTest,
  )
where

import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromJust)
import Data.Text (pack, replace, unpack)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    combineShellCommands,
  )
import SnapshotTest.ShellCommands (SnapshotTestContext (..))
import SnapshotTest.Snapshot
  ( SnapshotType (..),
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
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

data SnapshotTest = SnapshotTest
  { _snapshotTestName :: String,
    _snapshotTestCommandsBuilder :: ShellCommandBuilder SnapshotTestContext [ShellCommand]
  }

makeSnapshotTest :: String -> [ShellCommandBuilder SnapshotTestContext ShellCommand] -> SnapshotTest
makeSnapshotTest snapshotTestName snapshotTestCommandBuilders =
  SnapshotTest
    { _snapshotTestName = snapshotTestName,
      _snapshotTestCommandsBuilder = sequence snapshotTestCommandBuilders
    }

-- | Runs a snapshot test by executing snapshot test's shell commands and then
--  comparing the generated files to the previous "golden" (expected) version of those files.
runSnapshotTest :: SnapshotTest -> IO TestTree
runSnapshotTest snapshotTest = do
  let snapshotTestName = _snapshotTestName snapshotTest
  snapshotsAbsDir <- getSnapshotsDir

  let currentSnapshotAbsDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir snapshotTestName Current
  let goldenSnapshotAbsDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir snapshotTestName Golden
  let snapshotFileListManifestAbsFile = currentSnapshotAbsDir SP.</> snapshotFileListManifestFileInSnapshotDir

  let currentSnapshotDirAbsFp = SP.fromAbsDir currentSnapshotAbsDir
  let goldenSnapshotDirAbsFp = SP.fromAbsDir goldenSnapshotAbsDir
  let snapshotFileListManifestFileAbsFp = Sp.fromAbsFile snapshotFileListManifestAbsFile

  -- Remove existing current snapshot files from a prior test run.
  callCommand $ "rm -rf " ++ currentSnapshotDirAbsFp

  -- Create current snapshot dir as well as the golden snapshot dir, if missing.
  callCommand $ "mkdir " ++ currentSnapshotDirAbsFp
  callCommand $ "mkdir -p " ++ goldenSnapshotDirAbsFp

  let cdIntoCurrentSnapshotDirCommand = "cd " ++ currentSnapshotDirAbsFp
  let snapshotTestCommand = makeSnapshotTestCommand

  putStrLn $ "Running the following command: " ++ snapshotTestCommand
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ combineShellCommands [cdIntoCurrentSnapshotDirCommand, snapshotTestCommand]

  filesForCheckingExistenceAbsFps <- getFilesForCheckingExistence currentSnapshotDirAbsFp
  filesForCheckingContentAbsFps <- (snapshotFileListManifestFileAbsFp :) <$> getFilesForCheckingContent currentSnapshotDirAbsFp

  writeSnapshotFileListManifest currentSnapshotDirAbsFp filesForCheckingExistenceAbsFps snapshotFileListManifestFileAbsFp
  reformatPackageJsonFiles filesForCheckingContentAbsFps

  let remapCurrentToGoldenFilePath fp = unpack $ replace (pack currentSnapshotDirAbsFp) (pack goldenSnapshotDirAbsFp) (pack fp)

  return $
    testGroup
      snapshotTestName
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
    makeSnapshotTestCommand :: ShellCommand
    makeSnapshotTestCommand =
      let snapshotTestCommandsBuilder = _snapshotTestCommandsBuilder snapshotTest
       in combineShellCommands $ buildShellCommand SnapshotTestContext snapshotTestCommandsBuilder

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

    writeSnapshotFileListManifest :: String -> [FilePath] -> FilePath -> IO ()
    writeSnapshotFileListManifest baseAbsFp filesForCheckingExistenceAbsFps expectedFilesListAbsFp = do
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
