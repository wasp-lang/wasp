module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.WebAppGenerator.Auth.OAuthAuthG (genOAuthAuth)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Just auth ->
      sequence
        [ genLogout,
          genUseAuth,
          genCreateAuthRequiredPage auth,
          genUserHelpers
        ]
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
    Nothing -> return []
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates file with logout function to be used by Wasp developer.
genLogout :: Generator FileDraft
genLogout = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/logout.js|])

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
genUseAuth :: Generator FileDraft
genUseAuth = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/useAuth.js|])

genUserHelpers :: Generator FileDraft
genUserHelpers = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/helpers/user.ts|])
