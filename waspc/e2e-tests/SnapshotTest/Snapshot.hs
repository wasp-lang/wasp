module SnapshotTest.Snapshot
  ( SnapshotType (..),
  )
where

data SnapshotType = Golden | Current

instance Show SnapshotType where
  show Golden = "golden"
  show Current = "current"
