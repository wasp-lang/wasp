{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --modules=Tests/*Test.hs -optF --no-main -optF --generated-module=TestsDiscoverDriver #-}

-- Collects the tests (the `test_` definitions in the `Tests/*Test.hs` modules)
-- into `tests`, which `Main.hs` runs. Snapshot tests are in a subdirectory, so
-- `SnapshotTestsDiscoverDriver.hs` collects them instead.
