module Wasp.Generator.WebAppGenerator.AuthG
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
import Wasp.Generator.WebAppGenerator.Auth.AuthFormsG (genAuthForms)
import Wasp.Generator.WebAppGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.WebAppGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.WebAppGenerator.Auth.OAuthAuthG (genOAuthAuth)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ copyTmplFile [relfile|auth/logout.js|],
          copyTmplFile [relfile|auth/helpers/user.ts|],
          copyTmplFile [relfile|auth/types.ts|],
          genUseAuth auth,
          genCreateAuthRequiredPage auth
        ]
        <++> genAuthForms auth
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
        <++> genEmailAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    copyTmplFile = return . C.mkSrcTmplFd

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/pages/createAuthRequiredPage.jsx|]
      (object ["onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth])

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth = return $ C.mkTmplFdWithData [relfile|src/auth/useAuth.ts|] tmplData
  where
    tmplData = object ["entitiesGetMeDependsOn" .= makeJsArrayFromHaskellList [userEntityName]]
    userEntityName = AS.refName $ AS.Auth.userEntity auth
