module E2e.Test
  ( E2eTest (..),
    TestKind (..),
    test,
    snapshotTest,
    testGroup,
    usingResources,
    requiringDocker,
    fullTestName,
  )
where

import Control.Monad (void)
import Data.List (intercalate)
import E2e.Resource (Resource)
import E2e.TestM (TestM)

data E2eTest = E2eTest
  { name :: String,
    -- | Names of the groups this test is nested in, outermost first.
    groupPath :: [String],
    kind :: TestKind,
    resources :: [Resource],
    requiresDocker :: Bool,
    steps :: TestM ()
  }

data TestKind
  = -- | Passes when all of its steps succeed.
    PlainTest
  | -- | Passes when all of its steps succeed and the files they produce match
    -- the golden snapshot.
    SnapshotTest

test :: String -> TestM a -> E2eTest
test = makeTest PlainTest

-- | The test's steps run inside the snapshot directory. Everything they leave
-- behind is compared to the golden snapshot of the same name.
snapshotTest :: String -> TestM a -> E2eTest
snapshotTest = makeTest SnapshotTest

makeTest :: TestKind -> String -> TestM a -> E2eTest
makeTest testKind testName testSteps =
  E2eTest
    { name = testName,
      groupPath = [],
      kind = testKind,
      resources = [],
      requiresDocker = False,
      steps = void testSteps
    }

testGroup :: String -> [E2eTest] -> [E2eTest]
testGroup groupName = map $ \e2eTest -> e2eTest {groupPath = groupName : e2eTest.groupPath}

usingResources :: [Resource] -> E2eTest -> E2eTest
usingResources testResources e2eTest = e2eTest {resources = e2eTest.resources ++ testResources}

-- | Skips the test when Docker is disabled (see 'E2e.TestM.isDockerEnabled').
requiringDocker :: E2eTest -> E2eTest
requiringDocker e2eTest = e2eTest {requiresDocker = True}

fullTestName :: E2eTest -> String
fullTestName e2eTest = intercalate "/" (e2eTest.groupPath ++ [e2eTest.name])
