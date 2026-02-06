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
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Util ((<++>))

genAuthForms :: AS.Auth.Auth -> Generator [FileDraft]
genAuthForms auth =
  sequence
    [ genAuthComponent auth,
      genFileCopyInAuthForms [relfile|Auth.module.css|],
      genTypes auth,
      genFileCopyInAuthForms [relfile|Login.tsx|],
      genFileCopyInAuthForms [relfile|Signup.tsx|]
    ]
    <++> genEmailForms auth
    <++> genInternalAuthComponents auth

genAuthComponent :: AS.Auth.Auth -> Generator FileDraft
genAuthComponent auth =
  return $
    mkTmplFdWithData
      (authFormsDirInSdkTemplatesDir </> [relfile|Auth.tsx|])
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $
    mkTmplFdWithData
      (authFormsDirInSdkTemplatesDir </> [relfile|types.ts|])
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ genFileCopyInAuthForms [relfile|ResetPassword.tsx|],
        genFileCopyInAuthForms [relfile|ForgotPassword.tsx|],
        genFileCopyInAuthForms [relfile|VerifyEmail.tsx|]
      ]
  where
    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth

genInternalAuthComponents :: AS.Auth.Auth -> Generator [FileDraft]
genInternalAuthComponents auth =
  sequence
    [ genFileCopyInAuthFormsInternal [relfile|auth-styles.css|],
      genFileCopyInAuthFormsInternal [relfile|util.ts|]
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
        [ genFileCopyInAuthFormsInternal [relfile|Form.tsx|],
          genFileCopyInAuthFormsInternal [relfile|Form.module.css|]
        ]

    genMessageComponent =
      sequence
        [ genFileCopyInAuthFormsInternal [relfile|Message.tsx|],
          genFileCopyInAuthFormsInternal [relfile|Message.module.css|]
        ]

    genEmailComponents =
      genConditionally isEmailAuthEnabled $
        sequence
          [ genFileCopyInAuthFormsInternal [relfile|email/VerifyEmailForm.tsx|],
            genFileCopyInAuthFormsInternal [relfile|email/useEmail.ts|],
            genFileCopyInAuthFormsInternal [relfile|email/ForgotPasswordForm.tsx|],
            genFileCopyInAuthFormsInternal [relfile|email/ResetPasswordForm.tsx|]
          ]

    genUsernameAndPasswordComponents =
      genConditionally isUsernameAndPasswordAuthEnabled $
        sequence
          [ genFileCopyInAuthFormsInternal [relfile|usernameAndPassword/useUsernameAndPassword.ts|]
          ]

    genSocialComponents =
      genConditionally isExternalAuthEnabled $
        genSocialButtonComponent
          <++> genSocialIconsComponent

    genSocialButtonComponent =
      sequence
        [ genFileCopyInAuthFormsInternal [relfile|social/SocialButton.tsx|],
          genFileCopyInAuthFormsInternal [relfile|social/SocialButton.module.css|]
        ]

    genSocialIconsComponent =
      sequence
        [ genFileCopyInAuthFormsInternal [relfile|social/SocialIcons.tsx|],
          genFileCopyInAuthFormsInternal [relfile|social/SocialIcons.module.css|]
        ]

    isEmailAuthEnabled = AS.Auth.isEmailAuthEnabled auth
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth
    isExternalAuthEnabled = AS.Auth.isExternalAuthEnabled auth

genLoginSignupForm :: AS.Auth.Auth -> Generator [FileDraft]
genLoginSignupForm auth =
  sequence
    [ return $
        mkTmplFdWithData
          (authFormsInternalDirInSdkTemplatesDir </> [relfile|common/LoginSignupForm.tsx|])
          loginSignupFormComponentTmplData,
      genFileCopyInAuthFormsInternal [relfile|common/LoginSignupForm.module.css|]
    ]
  where
    loginSignupFormComponentTmplData =
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

authFormsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
authFormsDirInSdkTemplatesDir = [reldir|auth/forms|]

genFileCopyInAuthForms :: Path' Rel' File' -> Generator FileDraft
genFileCopyInAuthForms =
  genFileCopy . (authFormsDirInSdkTemplatesDir </>)

authFormsInternalDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
authFormsInternalDirInSdkTemplatesDir = authFormsDirInSdkTemplatesDir </> [reldir|internal|]

genFileCopyInAuthFormsInternal :: Path' Rel' File' -> Generator FileDraft
genFileCopyInAuthFormsInternal =
  genFileCopy . (authFormsInternalDirInSdkTemplatesDir </>)
