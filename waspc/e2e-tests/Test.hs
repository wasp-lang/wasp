{-# LANGUAGE DuplicateRecordFields #-}

module Test
  ( Test (..),
    TestCase (..),
    testTreeFromTest,
  )
where

import Context (TestContext (..), makeWaspProjectContext)
import FileSystem (getTestCaseDir)
import StrongPath (fromAbsDir)
import qualified System.Directory as SD
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import TestAction (TestAction, runTestAction)

data Test = Test
  { name :: String,
    testCases :: [TestCase]
  }

-- | Represent a single test case of some 'Test'.
data TestCase = TestCase
  { name :: String,
    actions :: TestAction TestContext ()
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
            waspProjectContext = makeWaspProjectContext testCaseDir
          }

  runTestAction testCaseContext testCase'.actions
