module Wasp.Cli.Util.EnvVars
  ( EnvVarSource,
    findWaspOwnedEnvVarsSetByUser,
    throwIfWaspOwnedEnvVarsAreSet,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Env (EnvVarName)
import Wasp.Project.Apps (Apps)
import Wasp.Util.Terminal (styleCode)

-- | Where the user set an env var, e.g. @".env.server"@ or @"the --server-env option"@.
type EnvVarSource = String

-- | Wasp injects the env vars it derives from the ports it picked into the processes it
-- starts, and injected values win over whatever the user wrote down. A value the user set
-- would therefore be silently ignored, so we stop instead.
throwIfWaspOwnedEnvVarsAreSet ::
  String ->
  Apps [EnvVarName] ->
  Apps [(EnvVarSource, [EnvVarName])] ->
  Command ()
throwIfWaspOwnedEnvVarsAreSet commandName waspOwnedEnvVarNames envVarNamesSetByUser =
  unless (null envVarsSetByUser) $
    throwError $
      CommandError "Wasp controls some of the env vars you set" $
        intercalate "\n" $
          [ printf
              "Wasp figures out the app's ports and URLs itself when you run %s, so it would ignore the values you set:"
              (styleCode commandName),
            ""
          ]
            ++ map describeEnvVarSetByUser envVarsSetByUser
            ++ [ "",
                 "Remove them, and let Wasp manage the ports and URLs for you."
               ]
  where
    envVarsSetByUser = findWaspOwnedEnvVarsSetByUser waspOwnedEnvVarNames envVarNamesSetByUser

    describeEnvVarSetByUser (envVarName, sources) =
      printf "  - %s, set in %s" (styleCode envVarName) (intercalate " and " sources)

-- | Which of the wasp-owned env var names did the user set, and where.
findWaspOwnedEnvVarsSetByUser ::
  Apps [EnvVarName] ->
  Apps [(EnvVarSource, [EnvVarName])] ->
  [(EnvVarName, [EnvVarSource])]
findWaspOwnedEnvVarsSetByUser waspOwnedEnvVarNames envVarNamesSetByUser =
  mergeSourcesPerEnvVar $
    concat $
      toList $
        findEnvVarsSetByUser <$> waspOwnedEnvVarNames <*> envVarNamesSetByUser
  where
    findEnvVarsSetByUser envVarNames namesPerSource =
      [ (envVarName, sources)
        | envVarName <- envVarNames,
          let sources = [source | (source, names) <- namesPerSource, envVarName `elem` names],
          not (null sources)
      ]

    -- The client and the server take their port through the same env var name, so a
    -- var can turn up once per app. We report it once, listing everywhere it came from.
    mergeSourcesPerEnvVar =
      Map.toAscList . Map.fromListWith (\newSources sources -> nub $ sources ++ newSources)
