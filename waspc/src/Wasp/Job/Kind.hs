module Wasp.Job.Kind (JobKind (..)) where

data JobKind = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)
