module Wasp.Project.Env
  ( readDotEnvFiles,
    warnIfTheDotEnvPresent,
    dotEnvFiles,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, relfile)
import Wasp.Env (EnvVar, parseDotEnvFile)
import Wasp.Project.Apps (Apps (..))
import Wasp.Project.Common (CompileWarning, WaspProjectDir, findFileInWaspProjectDir)

dotEnvFiles :: Apps (Path' (Rel WaspProjectDir) File')
dotEnvFiles =
  Apps
    { client = [relfile|.env.client|],
      server = [relfile|.env.server|]
    }

readDotEnvFiles :: Path' Abs (Dir WaspProjectDir) -> IO (Apps [EnvVar])
readDotEnvFiles waspDir = traverse (readDotEnvFileInWaspProjectDir waspDir) dotEnvFiles

-- | Checks if .env exists in wasp dir, and produces a warning if so.
-- We have this function because Wasp doesn't use ".env", but still user
-- might assume it does and then bother quite a bit trying to figure it out,
-- so this way we warn if they assume so.
warnIfTheDotEnvPresent :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe CompileWarning)
warnIfTheDotEnvPresent waspDir = (warningMessage <$) <$> findFileInWaspProjectDir waspDir [relfile|.env|]
  where
    warningMessage = "Wasp .env files should be named .env.server or .env.client, depending on their use."

-- Reads specified dotenv file and returns its values.
-- If file doesn't exist, returns an empty list.
-- If file can't be parsed, it will crash with an error.
readDotEnvFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO [(String, String)]
readDotEnvFileInWaspProjectDir waspDir envFileInWaspDir = do
  findFileInWaspProjectDir waspDir envFileInWaspDir >>= \case
    Nothing -> return []
    Just envFile -> parseDotEnvFile envFile
