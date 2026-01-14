module Wasp.Generator.Valid.PackageJson.Workspaces
  ( workspacesValidator,
  )
where

import Data.Bool (bool)
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Generator.NpmWorkspaces as NW
import qualified Wasp.Generator.Valid.Validator as V

type WorkspaceName = String

requiredWorkspaces :: [WorkspaceName]
requiredWorkspaces = S.toList NW.requiredWorkspaceGlobs

forbiddenWorkspaces :: [WorkspaceName]
forbiddenWorkspaces = [".wasp/build/*"]

workspacesValidator :: V.Validator P.PackageJson
workspacesValidator =
  V.inField ("workspaces", P.workspaces) $
    maybe noWorkspacesDefinedError $
      V.all
        [ requiredWorkspaceValidator,
          forbiddenWorkspaceValidator
        ]
  where
    requiredWorkspaceValidator :: V.Validator [WorkspaceName]
    requiredWorkspaceValidator =
      V.all $ makeWorkspaceIncludedValidator <$> requiredWorkspaces

    forbiddenWorkspaceValidator :: V.Validator [WorkspaceName]
    forbiddenWorkspaceValidator =
      V.all $ makeWorkspaceNotIncludedValidator <$> forbiddenWorkspaces

    noWorkspacesDefinedError =
      V.failure $
        "Wasp requires \"workspaces\" to have the value: ["
          ++ intercalate ", " (show <$> requiredWorkspaces)
          ++ "]."

makeWorkspaceIncludedValidator :: WorkspaceName -> V.Validator [WorkspaceName]
makeWorkspaceIncludedValidator expectedWorkspace =
  bool missingWorkspaceError V.success
    . elem expectedWorkspace
  where
    missingWorkspaceError =
      V.failure $
        "Wasp requires workspace"
          ++ show expectedWorkspace
          ++ " to be included."

makeWorkspaceNotIncludedValidator :: WorkspaceName -> V.Validator [WorkspaceName]
makeWorkspaceNotIncludedValidator forbiddenWorkspace =
  bool V.success forbiddenWorkspaceError
    . elem forbiddenWorkspace
  where
    forbiddenWorkspaceError =
      V.failure $
        "Wasp requires "
          ++ show forbiddenWorkspace
          ++ " not to be included."
