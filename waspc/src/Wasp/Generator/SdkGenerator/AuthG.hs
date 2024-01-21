module Wasp.Generator.SdkGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.AuthFormsG (genAuthForms)
import Wasp.Generator.SdkGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.SdkGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.SdkGenerator.Auth.OAuthAuthG (genOAuthAuth)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genFileCopy [relfile|auth/helpers/user.ts|],
          genFileCopy [relfile|auth/types.ts|],
          genFileCopy [relfile|auth/user.ts|],
          genFileCopy [relfile|auth/logout.ts|],
          genUseAuth auth
        ]
        <++> genAuthForms auth
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
        <++> genEmailAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkTmplFd

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth = return $ C.mkTmplFdWithData [relfile|auth/useAuth.ts|] tmplData
  where
    tmplData = object ["entitiesGetMeDependsOn" .= makeJsArrayFromHaskellList [userEntityName]]
    userEntityName = AS.refName $ AS.Auth.userEntity auth
