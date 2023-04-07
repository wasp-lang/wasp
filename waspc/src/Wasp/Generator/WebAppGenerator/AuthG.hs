module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import StrongPath (File', Path', Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders (gitHubAuthProvider, googleAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Auth.EmailAuthG (genEmailAuth)
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
          genUseAuth auth,
          genCreateAuthRequiredPage auth,
          genUserHelpers
        ]
        <++> genAuthForms auth
        <++> genLocalAuth auth
        <++> genOAuthAuth auth
        <++> genEmailAuth auth
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
genUseAuth :: AS.Auth.Auth -> Generator FileDraft
genUseAuth auth = return $ C.mkTmplFdWithData [relfile|src/auth/useAuth.ts|] tmplData
  where
    tmplData = object ["entitiesGetMeDependsOn" .= ("[" ++ userEntitiyStr ++ "]")]
    userEntitiyStr = "'" ++ userEntityName ++ "'"
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthForm auth,
      copyTmplFile [relfile|auth/forms/Login.tsx|],
      copyTmplFile [relfile|auth/forms/Signup.tsx|],
      copyTmplFile [relfile|auth/forms/ResetPassword.tsx|],
      copyTmplFile [relfile|auth/forms/ForgotPassword.tsx|],
      copyTmplFile [relfile|auth/forms/VerifyEmail.tsx|],
      copyTmplFile [relfile|auth/forms/types.ts|],
      copyTmplFile [relfile|stitches.config.js|],
      copyTmplFile [relfile|auth/forms/SocialIcons.tsx|],
      copyTmplFile [relfile|auth/forms/SocialButton.tsx|]
    ]
  where
    copyTmplFile = return . C.mkSrcTmplFd

genAuthForm :: AS.Auth.Auth -> Generator FileDraft
genAuthForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Auth.tsx|]
    [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
      "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
      "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
      "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
      -- Google
      "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth,
      "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
      -- GitHub
      "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth,
      "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider,
      -- Username and password
      "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
      -- Email
      "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth,
      "isEmailVerificationRequired" .= AS.Auth.isEmailVerificationRequired auth
    ]
  where
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth || AS.Auth.isEmailAuthEnabled auth

compileTmplToSamePath :: Path' Rel' File' -> [Pair] -> Generator FileDraft
compileTmplToSamePath tmplFileInTmplSrcDir keyValuePairs =
  return $
    C.mkTmplFdWithData
      (asTmplFile $ [reldir|src|] </> tmplFileInTmplSrcDir)
      (object keyValuePairs)

genUserHelpers :: Generator FileDraft
genUserHelpers = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/helpers/user.ts|])
