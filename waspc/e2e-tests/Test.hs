{-# LANGUAGE DuplicateRecordFields #-}

module Test
  ( Test (..),
    TestCase (..),
    testTreeFromTest,
  )
where

import Context (TestContext (..), WaspProjectContext (..))
import Data.Maybe (fromJust)
import FileSystem (getTestCaseDir, testCaseLogFileInTestCaseDir)
import Step (Step, runSteps)
import StrongPath (fromAbsDir, parseRelDir, (</>))
import qualified System.Directory as SD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    steps :: Step TestContext ()
  }

testTreeFromTest :: Test -> TestTree
testTreeFromTest test =
  testGroup test.name $
    map (\tc -> testCase tc.name $ runTestCase test tc) test.testCases

runTestCase :: Test -> TestCase -> Assertion
runTestCase test testCase' = do
  testCaseDir <- getTestCaseDir test.name testCase'.name

  -- Remove any leftovers of a previous run of this test case.
  SD.removePathForcibly $ fromAbsDir testCaseDir
  SD.createDirectoryIfMissing True $ fromAbsDir testCaseDir

  let testCaseContext =
        TestContext
          { testCaseDir,
            waspProjectContext =
              WaspProjectContext
                { waspProjectDir = testCaseDir </> (fromJust . parseRelDir $ "wasp-app"),
                  waspProjectName = "wasp-app"
                }
          }
      testName = test.name ++ " / " ++ testCase'.name
      logFile = testCaseDir </> testCaseLogFileInTestCaseDir

  result <- runSteps testName logFile testCaseContext testCase'.steps
  either assertFailure return result
