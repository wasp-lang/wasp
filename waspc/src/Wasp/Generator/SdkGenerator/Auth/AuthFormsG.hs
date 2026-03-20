module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as AesonKey
import Data.Char (toLower)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    allOAuthProviders,
    enabledAuthMethodsJson,
    isEmailEnabled,
    isOAuthEnabled,
    isUsernameAndPasswordEnabled,
    serverOAuthLoginUrl,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
-- todo(filip) -- Should I put this under something like Wasp.Generator.Auth (doesn't exist) or Wasp.Generator.Common?
import Wasp.Generator.SdkGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
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
    tmplData = object ["isEmailAuthEnabled" .= isEmailEnabled auth]

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth =
  return $
    mkTmplFdWithData
      (authFormsDirInSdkTemplatesDir </> [relfile|types.ts|])
      tmplData
  where
    tmplData = object ["isEmailAuthEnabled" .= isEmailEnabled auth]

genEmailForms :: AS.Auth.Auth -> Generator [FileDraft]
genEmailForms auth =
  genConditionally isEmailAuthEnabled $
    sequence
      [ genFileCopyInAuthForms [relfile|ResetPassword.tsx|],
        genFileCopyInAuthForms [relfile|ForgotPassword.tsx|],
        genFileCopyInAuthForms [relfile|VerifyEmail.tsx|]
      ]
  where
    isEmailAuthEnabled = isEmailEnabled auth

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

    isEmailAuthEnabled = isEmailEnabled auth
    isUsernameAndPasswordAuthEnabled = isUsernameAndPasswordEnabled auth
    isExternalAuthEnabled = isOAuthEnabled auth

genLoginSignupForm :: AS.Auth.Auth -> Generator [FileDraft]
genLoginSignupForm auth =
  sequence
    [ genLoginSigunFormComponent,
      genFileCopyInAuthFormsInternal [relfile|common/LoginSignupForm.module.css|]
    ]
  where
    genLoginSigunFormComponent =
      return $
        mkTmplFdWithData
          (authFormsInternalDirInSdkTemplatesDir </> [relfile|common/LoginSignupForm.tsx|])
          loginSignupFormComponentTmplData
    loginSignupFormComponentTmplData =
      object $
        [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
          "areBothSocialAndPasswordBasedAuthEnabled" .= areBothSocialAndPasswordBasedAuthEnabled,
          "isAnyPasswordBasedAuthEnabled" .= isAnyPasswordBasedAuthEnabled,
          "isSocialAuthEnabled" .= isOAuthEnabled auth,
          "enabledProviders" .= enabledAuthMethodsJson auth
        ]
          ++ [ AesonKey.fromString (lowerFirst (displayName spec) ++ "SignInPath") .= serverOAuthLoginUrl spec
               | spec <- allOAuthProviders
             ]
    areBothSocialAndPasswordBasedAuthEnabled = isOAuthEnabled auth && isAnyPasswordBasedAuthEnabled
    isAnyPasswordBasedAuthEnabled = isUsernameAndPasswordEnabled auth || isEmailEnabled auth
    lowerFirst (c : cs) = toLower c : cs
    lowerFirst [] = []

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
