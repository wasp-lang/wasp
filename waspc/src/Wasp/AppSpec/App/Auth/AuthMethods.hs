{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Wasp.AppSpec.App.Auth.AuthMethods
  ( generateAuthMethods,
    userSignupFieldsForEmailAuth,
    userSignupFieldsForUsernameAuth,
    userSignupFieldsForExternalAuth,
    isAuthMethodExternal,
    AuthMethod (..),
    UsernameAndPasswordConfig (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
  )
where

import Data.Data (Data)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarBangType)
import Wasp.AppSpec.App.Auth.EmailVerification (EmailVerificationConfig)
import Wasp.AppSpec.App.Auth.PasswordReset (PasswordResetConfig)
import Wasp.AppSpec.App.EmailSender (EmailFromField)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.Util (toLowerFirst)

data AuthMethod = UsernameAndPassword | Email | Google | Keycloak | GitHub deriving (Show, Enum, Bounded)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)

data EmailAuthConfig = EmailAuthConfig
  { userSignupFields :: Maybe ExtImport,
    fromField :: EmailFromField,
    emailVerification :: EmailVerificationConfig,
    passwordReset :: PasswordResetConfig
  }
  deriving (Show, Eq, Data)

configType :: AuthMethod -> Name
configType UsernameAndPassword = ''UsernameAndPasswordConfig
configType Email = ''EmailAuthConfig
configType Google = ''ExternalAuthConfig
configType Keycloak = ''ExternalAuthConfig
configType GitHub = ''ExternalAuthConfig

-- Generate the AuthMethods data type
-- data AuthMethods = AuthMethods
--   { usernameAndPassword :: Maybe UsernameAndPasswordConfig,
--     google :: Maybe ExternalAuthConfig,
--     gitHub :: Maybe ExternalAuthConfig,
--     keycloak :: Maybe ExternalAuthConfig,
--     email :: Maybe EmailAuthConfig
--     ...
--   } deriving (Show, Eq, Data)
generateAuthMethods :: Q [Dec]
generateAuthMethods = do
  let authMethodsName = mkName "AuthMethods"
  let authMethodsCtorName = mkName "AuthMethods"
  let fields = generateField <$> [UsernameAndPassword .. GitHub]
  -- data AuthMethods
  let authMethods =
        dataD
          (cxt [])
          authMethodsName
          []
          Nothing
          [recC authMethodsCtorName fields]
          [derivClause Nothing [[t|Show|], [t|Eq|], [t|Data|]]]
  sequence [authMethods]
  where
    -- usernameAndPassword :: Maybe UsernameAndPasswordConfig
    generateField :: AuthMethod -> Q VarBangType
    generateField authMethod = do
      let fieldName = mkName (toLowerFirst (show authMethod))
      let fieldConfigType = configType authMethod
      let fieldType = appT (conT ''Maybe) (conT fieldConfigType)
      let fieldStrictness = bang noSourceUnpackedness noSourceStrictness
      varBangType fieldName $ bangType fieldStrictness fieldType

-- These helper functions are used to avoid ambiguity when using the
-- `userSignupFields` function (otherwise we need to use the DuplicateRecordFields
-- extension in each module that uses them).
userSignupFieldsForEmailAuth :: EmailAuthConfig -> Maybe ExtImport
userSignupFieldsForEmailAuth = userSignupFields

userSignupFieldsForUsernameAuth :: UsernameAndPasswordConfig -> Maybe ExtImport
userSignupFieldsForUsernameAuth = userSignupFields

userSignupFieldsForExternalAuth :: ExternalAuthConfig -> Maybe ExtImport
userSignupFieldsForExternalAuth = userSignupFields

isAuthMethodExternal :: AuthMethod -> Bool
isAuthMethodExternal Google = True
isAuthMethodExternal Keycloak = True
isAuthMethodExternal GitHub = True
isAuthMethodExternal _ = False
