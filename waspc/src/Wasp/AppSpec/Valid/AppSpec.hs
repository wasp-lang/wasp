module Wasp.AppSpec.Valid.AppSpec
  ( validateAppSpec,
  )
where

import Wasp.AppSpec
import Wasp.AppSpec.Valid.Internal (Valid (..))

-- TODO: Separate this one into its own file.
data ValidationError = GenericValidationError String

-- TODO: add more checks?
validateAppSpec :: AppSpec -> Either ValidationError (Valid AppSpec)
validateAppSpec spec =
  case getDecls @App spec of
    [app] -> return ()
    apps -> Left $ GenericValidationError "Expected exactly 1 'app' declaration, but found " ++ show (length apps)
  return $ Valid spec
