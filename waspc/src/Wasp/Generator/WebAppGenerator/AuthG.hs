module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import StrongPath (relfile, Path', Rel', File', reldir, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.AuthProviders (gitHubAuthProvider, googleAuthProvider)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
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
        <++> genAuthForms auth
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

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genLoginForm auth,
      genSignupForm auth,
      genAuthForm auth,
      genStitchesConfig,
      genSocialIcons
    ]

genSocialIcons :: Generator FileDraft
genSocialIcons =
    copyTmplFile [relfile|auth/forms/SocialIcons.jsx|]
  where
    copyTmplFile = return . C.mkSrcTmplFd

genStitchesConfig :: Generator FileDraft
genStitchesConfig =
    -- TODO(matija): where should I put this? Maybe keep it localy scopped in auth/forms for now?
    copyTmplFile [relfile|stitches.config.js|]
  where
    -- TODO(matija): duplicated, extract
    copyTmplFile = return . C.mkSrcTmplFd

genAuthForm :: AS.Auth.Auth -> Generator FileDraft
genAuthForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Auth.jsx|]
    [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth
    , "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth
    , "isBothExternalAndUsernameAndPasswordAuthEnabled" .= AS.Auth.isBothExternalAndUsernameAndPasswordAuthEnabled auth
       -- Social auth methods
    , "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth
    -- Google
    , "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth
    , "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider
    -- GitHub
    , "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth
    , "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider
    ]

genLoginForm :: AS.Auth.Auth -> Generator FileDraft
genLoginForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Login.jsx|]
    [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth
    ]

genSignupForm :: AS.Auth.Auth -> Generator FileDraft
genSignupForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Signup.jsx|]
    [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth
    ]

compileTmplToSamePath :: Path' Rel' File' -> [Pair] -> Generator FileDraft
compileTmplToSamePath tmplFileInTmplSrcDir keyValuePairs =
  return $
    C.mkTmplFdWithDstAndData
      (asTmplFile $ [reldir|src|] </> tmplFileInTmplSrcDir)
      targetPath
      (Just templateData)
  where
    targetPath = C.webAppSrcDirInWebAppRootDir </> asWebAppSrcFile tmplFileInTmplSrcDir
    templateData = object keyValuePairs

genUserHelpers :: Generator FileDraft
genUserHelpers = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/helpers/user.ts|])
