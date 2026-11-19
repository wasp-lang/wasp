module Wasp.Generator.Valid.PackageJson.Workspaces
  ( workspacesValidator,
  )
where

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Project.Common (dotWaspDirInWaspProjectDir)
import qualified Wasp.Validator as V

type WorkspaceGlob = String

-- | The generated code used to be npm workspaces of the project, and projects
-- from before Wasp 0.27 still list them. The project now depends on the Wasp
-- SDK directly (see "Wasp.Generator.Valid.PackageJson.Dependencies").
workspacesValidator :: V.Validator P.PackageJson
workspacesValidator =
  V.inField ("workspaces", P.workspaces) $
    V.ifJust notGeneratedByWaspValidator
  where
    notGeneratedByWaspValidator :: V.Validator [WorkspaceGlob]
    notGeneratedByWaspValidator workspaceGlobs =
      case filter isInDotWaspDir workspaceGlobs of
        [] -> V.success
        generatedWorkspaceGlobs ->
          V.failure $
            "Wasp's generated code is no longer an npm workspace. Remove "
              ++ intercalate ", " (show <$> generatedWorkspaceGlobs)
              ++ " from \"workspaces\"."

    isInDotWaspDir :: WorkspaceGlob -> Bool
    isInDotWaspDir workspaceGlob = any (`isPrefixOf` workspaceGlob) [dotWaspDir, "./" ++ dotWaspDir]

    -- npm writes workspace globs with forward slashes on every platform.
    dotWaspDir = SP.fromRelDirP $ fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir
