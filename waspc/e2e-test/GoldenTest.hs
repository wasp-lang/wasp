module GoldenTest
  ( runGoldenTest,
    makeGoldenTest,
    GoldenTest,
  )
where

import Common (getTestOutputsDir)
import Control.Monad (filterM)
import Data.Aeson (Value, decode)
import Data.Aeson.Encode.Pretty
  ( Config (Config, confCompare, confIndent, confNumFormat, confTrailingNewline),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
  )
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromJust)
import Data.Text (pack, replace, unpack)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    ShellCommandContext (..),
    combineShellCommands,
    runShellCommandBuilder,
  )
import qualified StrongPath as SP
import System.Directory (doesFileExist, renameFile)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (makeRelative, takeFileName)
import qualified System.FilePath as FP
import System.Process (callCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

data GoldenTest = GoldenTest {_goldenTestName :: String, _goldenTestCommands :: ShellCommandBuilder [ShellCommand]}

makeGoldenTest :: String -> ShellCommandBuilder [ShellCommand] -> GoldenTest
makeGoldenTest name commands = GoldenTest {_goldenTestName = name, _goldenTestCommands = commands}

-- | This runs a golden test by creating a Wasp project (via `wasp-cli new`), running commands,
-- and then comparing all file outputs to the corresponding golden test output directory.
runGoldenTest :: GoldenTest -> IO TestTree
runGoldenTest goldenTest = do
  testOutputsDirAbsSp <- getTestOutputsDir
  let testOutputsDirAbsFp = SP.fromAbsDir testOutputsDirAbsSp
  let currentOutputDirAbsFp = testOutputsDirAbsFp FP.</> (_goldenTestName goldenTest ++ "-current")
  let goldenOutputDirAbsFp = testOutputsDirAbsFp FP.</> (_goldenTestName goldenTest ++ "-golden")

  -- Remove existing current output files from a prior test run.
  callCommand $ "rm -rf " ++ currentOutputDirAbsFp

  -- Create current output dir as well as the golden output dir, if missing.
  callCommand $ "mkdir " ++ currentOutputDirAbsFp
  callCommand $ "mkdir -p " ++ goldenOutputDirAbsFp

  let context =
        ShellCommandContext
          { _ctxtCurrentProjectName = _goldenTestName goldenTest
          }
  let shellCommand = combineShellCommands $ runShellCommandBuilder (_goldenTestCommands goldenTest) context
  putStrLn $ "Running the following command: " ++ shellCommand

  -- Run the series of commands within the context of a current output dir.
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ "cd " ++ currentOutputDirAbsFp ++ " && " ++ shellCommand

  currentOutputAbsFps <- listRelevantTestOutputFiles currentOutputDirAbsFp
  reformatPackageJsonFiles currentOutputAbsFps

  let manifestAbsFp = currentOutputDirAbsFp FP.</> "files.manifest"
  writeFileManifest currentOutputDirAbsFp currentOutputAbsFps manifestAbsFp

  let remapCurrentPathToGolden fp = unpack $ replace (pack currentOutputDirAbsFp) (pack goldenOutputDirAbsFp) (pack fp)

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
      takeFileName fp `notElem` [".waspinfo", "node_modules", "dev.db", "dev.db-journal", "package-lock.json", ".gitignore"]

    writeFileManifest :: String -> [FilePath] -> FilePath -> IO ()
    writeFileManifest baseAbsFp filePaths manifestAbsFp = do
      let sortedRelativeFilePaths = unlines . sort . map (makeRelative baseAbsFp) $ filePaths
      writeFile manifestAbsFp sortedRelativeFilePaths

    -- This function normalizes all package.json files for comparison as `npm install` can overwrite
    -- them when creating/updating package-lock.json. Different versions of npm may produce different
    -- (but semantically equivalent) package.json files, thus triggering false positives.
    -- Ref: https://github.com/wasp-lang/wasp/issues/482
    reformatPackageJsonFiles :: [FilePath] -> IO ()
    reformatPackageJsonFiles allOutputFilePaths = do
      let packageJsonFilePathSuffix = FP.pathSeparator : "package.json"
      let packageJsonFilePaths = filter (packageJsonFilePathSuffix `isSuffixOf`) allOutputFilePaths
      mapM_ reformatJson packageJsonFilePaths
      where
        aesonPrettyConfig = Config {confIndent = Spaces 4, confCompare = compare, confNumFormat = Generic, confTrailingNewline = True}
        reformatJson jsonFilePath = do
          let tmpFilePath = jsonFilePath ++ ".tmp"
          str <- BSL.readFile jsonFilePath
          -- NOTE: Aeson.decode into (:: Maybe Value) allows us to decode any
          -- valid JSON string, without specifying a schema.
          let json = fromJust (decode str :: Maybe Value)
          let formattedJson = encodePretty' aesonPrettyConfig json
          BSL.writeFile tmpFilePath formattedJson
          renameFile tmpFilePath jsonFilePath
