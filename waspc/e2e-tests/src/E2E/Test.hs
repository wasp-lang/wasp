-- | Declares tests: names them, puts them in groups, and decides where they
-- run. What a test does when it runs is its steps (see "E2E.Steps").
module E2E.Test
  ( TestTree,
    test,
    skippedTest,
    testGroup,
    sequentialTestGroup,
  )
where

import Data.List (intercalate)
import Data.Tagged (Tagged (..))
import E2E.Steps (TestFailure (..), TestM, TestPaths (..), runTestSteps)
import E2E.Suite (TestSuiteEnv (..), claimOutputDir, withTestSuiteEnv)
import System.FilePath (joinPath, (</>))
import Test.Tasty (TestTree, adjustOption, askOption)
import qualified Test.Tasty as Tasty
import Test.Tasty.Options (IsOption (..))
import Test.Tasty.Providers (IsTest (..), Result, singleTest, testFailed, testPassed)
import UnliftIO.Exception (catch)

-- | The names of the e2e groups a test is in, outermost first, which decide
-- where it runs (see 'test').
newtype GroupNames = GroupNames [String]

instance IsOption GroupNames where
  defaultValue = GroupNames []
  parseValue = const Nothing
  optionName = Tagged "e2e-group-names"
  optionHelp = Tagged "The e2e groups a test is in. Not a command line option."

-- | A test that passes when all of its steps succeed.
--
-- It runs in a fresh @test-outputs/<groups>/<name>/@ dir, and writes the
-- output of the commands it runs to @output.log@ in it.
test :: String -> TestM a -> TestTree
test name steps =
  withTestSuiteEnv $ \testSuiteEnv -> inTestDir name $ \testPath -> TestSteps $ do
    let testDir = testSuiteEnv.testOutputsDir </> joinPath testPath
        testPaths = TestPaths {workingDir = testDir, logFile = testDir </> testLogFileName}
    (runTestSteps testSuiteEnv (intercalate "/" testPath) testPaths steps >> return (testPassed ""))
      `catch` (\(TestFailure message) -> return $ testFailed message)

-- | A test that passes with the message, without running. It still claims the
-- dir it would run in, so that the dir check doesn't depend on which tests a
-- run skips.
skippedTest :: String -> String -> TestTree
skippedTest name message = inTestDir name . const . TestSteps . return $ testPassed message

-- | A single test that runs in @test-outputs/<groups>/<name>/@. Gets the
-- names of the dirs on the way.
inTestDir :: String -> ([String] -> TestSteps) -> TestTree
inTestDir name makeTestSteps =
  askOption $ \(GroupNames groupNames) ->
    let testPath = groupNames ++ [name]
     in claimOutputDir testPath . singleTest name $ makeTestSteps testPath

testLogFileName :: FilePath
testLogFileName = "output.log"

-- | Tests in the group run in parallel.
testGroup :: String -> [TestTree] -> TestTree
testGroup name = inGroup name . Tasty.testGroup name

-- | Tests in the group run one after another, in order. For tests that can't
-- run at the same time, e.g. because they use the same port.
sequentialTestGroup :: String -> [TestTree] -> TestTree
sequentialTestGroup name = inGroup name . Tasty.inOrderTestGroup name

inGroup :: String -> TestTree -> TestTree
inGroup name = adjustOption $ \(GroupNames groupNames) -> GroupNames (groupNames ++ [name])

-- | A tasty test that runs the IO action to get its result.
newtype TestSteps = TestSteps (IO Result)

instance IsTest TestSteps where
  run _ (TestSteps steps) _ = steps
  testOptions = pure []
