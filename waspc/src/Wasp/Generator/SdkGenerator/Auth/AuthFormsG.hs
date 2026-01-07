module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    slackAuthProvider,
  )
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkProject (..),
    SdkTemplatesProjectDir,
    makeSdkProjectTmplFd,
    makeSdkProjectTmplFdWithData,
  )
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthFormsFileCopy UserCoreProject [relfile|Login.tsx|],
      genAuthFormsFileCopy UserCoreProject [relfile|Signup.tsx|],
      genAuthFormsFileCopy UserCoreProject [relfile|Auth.module.css|],
      genAuthComponent auth,
      genTypes auth
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents auth

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $ makeSdkProjectTmplFdWithData UserCoreProject tmplFile tmplData
  where
    tmplFile = authFormsDirInSdkTemplatesProjectDir </> [relfile|Auth.tsx|]
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $ makeSdkProjectTmplFdWithData UserCoreProject tmplFile tmplData
  where
    tmplFile = authFormsDirInSdkTemplatesProjectDir </> [relfile|types.ts|]
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ genAuthFormsFileCopy UserCoreProject [relfile|ResetPassword.tsx|],
        genAuthFormsFileCopy UserCoreProject [relfile|ForgotPassword.tsx|],
        genAuthFormsFileCopy UserCoreProject [relfile|VerifyEmail.tsx|]
      ]
  where
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ genAuthFormsFileCopy UserCoreProject [relfile|internal/auth-styles.css|],
      genAuthFormsFileCopy UserCoreProject [relfile|internal/util.ts|]
    ]
    <++> genLoginSignupForm auth
    <++> genFormComponent
    <++> genMessageComponent
    <++> genEmailComponents
    <++> genUsernameAndPasswordComponents
    <++> genSocialComponents
  where
    genFormComponent =
      sequence
        [ genAuthFormsFileCopy UserCoreProject [relfile|internal/Form.tsx|],
          genAuthFormsFileCopy UserCoreProject [relfile|internal/Form.module.css|]
        ]

    genMessageComponent =
      sequence
        [ genAuthFormsFileCopy UserCoreProject [relfile|internal/Message.tsx|],
          genAuthFormsFileCopy UserCoreProject [relfile|internal/Message.module.css|]
        ]

    genEmailComponents =
      genConditionally isEmailAuthEnabled $
        sequence
          [ genAuthFormsFileCopy UserCoreProject [relfile|internal/email/VerifyEmailForm.tsx|],
            genAuthFormsFileCopy UserCoreProject [relfile|internal/email/useEmail.ts|],
            genAuthFormsFileCopy UserCoreProject [relfile|internal/email/ForgotPasswordForm.tsx|],
            genAuthFormsFileCopy UserCoreProject [relfile|internal/email/ResetPasswordForm.tsx|]
          ]
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

    genUsernameAndPasswordComponents =
      genConditionally isUsernameAndPasswordAuthEnabled $
        sequence
          [ genAuthFormsFileCopy UserCoreProject [relfile|internal/usernameAndPassword/useUsernameAndPassword.ts|]
          ]
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

    genSocialComponents =
      genConditionally isExternalAuthEnabled $
        genSocialButtonComponent
          <++> genSocialIconsComponent
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

    genSocialButtonComponent =
      sequence
        [ genAuthFormsFileCopy UserCoreProject [relfile|internal/social/SocialButton.tsx|],
          genAuthFormsFileCopy UserCoreProject [relfile|internal/social/SocialButton.module.css|]
        ]

    genSocialIconsComponent =
      sequence
        [ genAuthFormsFileCopy UserCoreProject [relfile|internal/social/SocialIcons.tsx|],
          genAuthFormsFileCopy UserCoreProject [relfile|internal/social/SocialIcons.module.css|]
        ]

genLoginSignupForm :: AS.Auth.Auth -> Generator [FileDraft]
genLoginSignupForm auth =
  sequence
    [ return $ makeSdkProjectTmplFdWithData UserCoreProject tmplFile tmplData,
      genAuthFormsFileCopy UserCoreProject [relfile|internal/common/LoginSignupForm.module.css|]
    ]
  where
    tmplFile = authFormsDirInSdkTemplatesProjectDir </> [relfile|internal/common/LoginSignupForm.tsx|]
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
          "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
          "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
          "isSocialAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
          "slackSignInPath" .= OAuth.serverLoginUrl slackAuthProvider,
          "discordSignInPath" .= OAuth.serverLoginUrl discordAuthProvider,
          "googleSignInPath" .= OAuth.serverLoginUrl googleAuthProvider,
          "keycloakSignInPath" .= OAuth.serverLoginUrl keycloakAuthProvider,
          "gitHubSignInPath" .= OAuth.serverLoginUrl gitHubAuthProvider,
          "enabledProviders" .= AuthProviders.getEnabledAuthProvidersJson auth
        ]
    areBothSocialAndPasswordBasedAuthEnabled = AS.Auth.isExternalAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth || AS.Auth.isEmailAuthEnabled auth

genConditionally :: Bool -> Generator [FileDraft] -> Generator [FileDraft]
genConditionally isEnabled gen = if isEnabled then gen else return []

authFormsDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
authFormsDirInSdkTemplatesProjectDir = [reldir|auth/forms|]

genAuthFormsFileCopy :: SdkProject -> Path' Rel' File' -> Generator FileDraft
genAuthFormsFileCopy sdkProject =
  return
    . makeSdkProjectTmplFd sdkProject
    . (authFormsDirInSdkTemplatesProjectDir </>)
