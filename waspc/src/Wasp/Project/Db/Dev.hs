-- | This module and modules under it capture what Wasp knows about its dev database and running it.
module Wasp.Project.Db.Dev
  ( makeDevDbUniqueId,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util as U

-- Returns a unique id that can be used for global identification of dev db tied to
-- a specific Wasp project.
-- Id is no longer than 30 chars, all of them ascii letters, numbers, hyphen or underscore.
-- It is designed this way to make it easily usable in many scenarios.
-- It contains app name (or big part of it), to make it also readable for humans.
-- It is not resistant to Wasp project moving or being renamed, and will change in that case.
-- TODO: Consider making this more general, make it a unique id of a project that can be used to
--   track (global) resources. So it would be not just for the db, but for anything.
makeDevDbUniqueId :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevDbUniqueId waspProjectDir appName = take 19 sanitizedAppName <> "-" <> take 10 projectPathHash
  where
    projectPathHash = U.hexToString $ U.checksumFromString $ fromAbsDir waspProjectDir
    sanitizedAppName = filter isSafeChar appName
    isSafeChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'
