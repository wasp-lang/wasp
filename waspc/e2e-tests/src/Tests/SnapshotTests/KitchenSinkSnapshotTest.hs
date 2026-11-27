module Tests.SnapshotTests.KitchenSinkSnapshotTest (test_kitchenSinkSnapshot) where

import E2E
import Tests.Fixtures (withKitchenSinkProject)

test_kitchenSinkSnapshot :: TestTree
test_kitchenSinkSnapshot =
  snapshotTest "kitchen-sink" $
    withKitchenSinkProject $ do
      wasp ["install"]
      wasp ["compile"]
