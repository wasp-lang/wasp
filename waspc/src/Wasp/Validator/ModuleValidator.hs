module Wasp.Validator.ModuleValidator
  ( validateModuleEntityMaps,
    ValidationError,
  )
where

import qualified Data.Map as Map
import Wasp.AppSpec.Module (EntityDeclaration (..))

type ValidationError = String

-- | Validates that entity maps provided by a host app satisfy the module's
-- entity declarations. Checks:
--   1. Every declared entity alias has a mapping in the entityMap
--   2. The mapped entity names are non-empty
--   3. If the module requiresAuth, the host must have auth configured
validateModuleEntityMaps ::
  String ->
  Bool ->
  [EntityDeclaration] ->
  Map.Map String String ->
  Bool ->
  [ValidationError]
validateModuleEntityMaps pkgName requiresAuth entityDecls entityMap hostHasAuth =
  concatMap (validateDeclaration pkgName entityMap) entityDecls
    ++ validateAuthRequirement pkgName requiresAuth hostHasAuth

validateDeclaration :: String -> Map.Map String String -> EntityDeclaration -> [ValidationError]
validateDeclaration pkgName entityMap decl =
  case Map.lookup (edName decl) entityMap of
    Nothing ->
      [ "Module '"
          ++ pkgName
          ++ "' declares entity '"
          ++ edName decl
          ++ "' but no mapping was provided. "
          ++ "Add '"
          ++ edName decl
          ++ "' to the entityMap in app.use()."
      ]
    Just realName
      | null realName ->
          [ "Module '"
              ++ pkgName
              ++ "' has an empty entity mapping for '"
              ++ edName decl
              ++ "'. Provide a valid entity name."
          ]
    Just _ -> []

validateAuthRequirement :: String -> Bool -> Bool -> [ValidationError]
validateAuthRequirement pkgName requiresAuth hostHasAuth
  | requiresAuth && not hostHasAuth =
      [ "Module '"
          ++ pkgName
          ++ "' requires auth but app.auth() is not configured."
      ]
  | otherwise = []
