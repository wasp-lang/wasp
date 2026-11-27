{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --modules=Tests/SnapshotTests/*Test.hs -optF --no-main -optF --generated-module=SnapshotTestsDiscoverDriver #-}

-- Collects the snapshot tests (the `test_` definitions in the
-- `Tests/SnapshotTests/*Test.hs` modules) into `tests`, which `Main.hs` runs.
