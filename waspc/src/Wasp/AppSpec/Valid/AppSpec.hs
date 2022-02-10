{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Valid.AppSpec
  ( validateAppSpec,
    getApp,
    isAuthEnabled,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec, getDecls)
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import Wasp.AppSpec.Valid (fromValid, ($^))
import Wasp.AppSpec.Valid.Internal (Valid (MakeValid))

-- TODO: Separate this one into its own file.
data ValidationError = GenericValidationError String
  deriving (Eq, Show)

-- TODO: add more checks?
validateAppSpec :: AppSpec -> Either [ValidationError] (Valid AppSpec)
validateAppSpec spec = do
  case getDecls @App spec of
    [_] -> return ()
    apps -> Left [GenericValidationError $ "Expected exactly 1 'app' declaration, but found " ++ show (length apps)]
  return $ MakeValid spec

getApp :: Valid AppSpec -> Valid (String, App)
getApp spec =
  let apps = getDecls @App <$> spec
   in case fromValid apps of
        [_] -> head <$> apps
        apps' ->
          error $
            "Expected exactly 1 'app' declaration in Valid AppSpec, but found " ++ show (length apps')
              ++ ". This should never happen, as this should have been already caught during AppSpec validation."

isAuthEnabled :: Valid AppSpec -> Bool
isAuthEnabled spec = isJust (App.auth $ snd $^ getApp spec)
