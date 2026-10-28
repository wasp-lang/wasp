-- | Everything needed to write e2e tests.
module E2e
  ( module E2e.Assertions,
    module E2e.Files,
    module E2e.Process,
    module E2e.Resource,
    module E2e.Test,
    module E2e.TestM,
    module E2e.Wasp,
  )
where

import E2e.Assertions
import E2e.Files
import E2e.Process
import E2e.Resource (Resource, devDatabasePort, exclusiveResource, prismaStudioPort, waspDevServerPorts, waspStudioPort)
import E2e.Test (E2eTest, requiringDocker, snapshotTest, test, testGroup, usingResources)
import E2e.TestM (TestM, failTest, inDirectory, logLine, registerCleanupStep, resolvePath, whenDockerEnabled)
import E2e.Wasp
