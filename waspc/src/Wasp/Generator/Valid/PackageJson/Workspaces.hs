module Wasp.Generator.Valid.PackageJson.Workspaces
  ( workspacesValidator,
  )
where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Generator.NpmWorkspaces as NW
import qualified Wasp.Generator.Valid.Validator as V

type WorkspaceName = String

requiredWorkspaces :: [WorkspaceName]
requiredWorkspaces = S.toList NW.requiredWorkspaceGlobs

workspacesValidator :: V.Validator P.PackageJson
workspacesValidator =
  V.inField ("workspaces", P.workspaces) $
    V.all [requiredWorkspaceValidator]
      -- We treat a missing workspaces field as an empty list, for validation purposes.
      -- This way, we can show errors about missing required workspaces.
      -- Using anything else than an array here would be caught by earlier JSON schema validation.
      . fromMaybe []
  where
    requiredWorkspaceValidator :: V.Validator [WorkspaceName]
    requiredWorkspaceValidator =
      V.all $ makeWorkspaceIncludedValidator <$> requiredWorkspaces

makeWorkspaceIncludedValidator :: WorkspaceName -> V.Validator [WorkspaceName]
makeWorkspaceIncludedValidator expectedWorkspace =
  bool missingWorkspaceError V.success
    . elem expectedWorkspace
  where
    missingWorkspaceError =
      V.failure $
        "Wasp requires "
          ++ show expectedWorkspace
          ++ " to be included."
