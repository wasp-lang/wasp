module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import StrongPath (File', Path', Rel', reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders.OAuth (ExternalAuthInfo, gitHubAuthInfo, googleAuthInfo)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Just auth ->
      sequence
        [ genSignup,
          genLogin,
          genLogout,
          genUseAuth,
          genCreateAuthRequiredPage auth,
          genUserHelpers
        ]
        <++> genAuthForms auth
    Nothing -> return []
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates file with signup function to be used by Wasp developer.
genSignup :: Generator FileDraft
genSignup = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/signup.js|])

-- | Generates file with login function to be used by Wasp developer.
genLogin :: Generator FileDraft
genLogin = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/login.js|])

-- | Generates file with logout function to be used by Wasp developer.
genLogout :: Generator FileDraft
genLogout = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/logout.js|])

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  compileTmplToSamePath
    [relfile|auth/pages/createAuthRequiredPage.js|]
    ["onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth]

-- | Generates React hook that Wasp developer can use in a component to get
--   access to the currently logged in user (and check whether user is logged in
--   ot not).
genUseAuth :: Generator FileDraft
genUseAuth = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/useAuth.js|])

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genLoginForm auth,
      genSignupForm auth
    ]
    <++> genExternalAuth auth

genLoginForm :: AS.Auth.Auth -> Generator FileDraft
genLoginForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Login.js|]
    ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth]

genSignupForm :: AS.Auth.Auth -> Generator FileDraft
genSignupForm auth =
  compileTmplToSamePath
    [relfile|auth/forms/Signup.js|]
    ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth]

genExternalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genExternalAuth auth
  | AS.App.Auth.isExternalAuthEnabled auth = (:) <$> genOAuthCodeExchange auth <*> genSocialLoginHelpers auth
  | otherwise = return []

genSocialLoginHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genSocialLoginHelpers auth =
  return $
    concat
      [ [gitHubHelpers | AS.App.Auth.isGitHubAuthEnabled auth],
        [googleHelpers | AS.App.Auth.isGoogleAuthEnabled auth]
      ]
  where
    gitHubHelpers = mkHelpersFd gitHubAuthInfo [relfile|GitHub.js|]
    googleHelpers = mkHelpersFd googleAuthInfo [relfile|Google.js|]

    mkHelpersFd :: ExternalAuthInfo -> Path' Rel' File' -> FileDraft
    mkHelpersFd externalAuthInfo helpersFp =
      mkTmplFdWithDstAndData
        [relfile|src/auth/helpers/Generic.js|]
        (SP.castRel $ [reldir|src/auth/helpers|] SP.</> helpersFp)
        (Just tmplData)
      where
        tmplData =
          object
            [ "signInPath" .= OAuth.serverLoginUrl externalAuthInfo,
              "iconName" .= SP.toFilePath (OAuth.logoFileName externalAuthInfo),
              "displayName" .= OAuth.displayName externalAuthInfo
            ]

genOAuthCodeExchange :: AS.Auth.Auth -> Generator FileDraft
genOAuthCodeExchange auth =
  compileTmplToSamePath
    [relfile|auth/pages/OAuthCodeExchange.js|]
    [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
      "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth
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

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)

genUserHelpers :: Generator FileDraft
genUserHelpers = return $ C.mkTmplFd (C.asTmplFile [relfile|src/auth/helpers/user.ts|])
