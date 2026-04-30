{-# LANGUAGE DuplicateRecordFields #-}

module Generator.Auth.ProviderTest where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import Test.Hspec
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import Wasp.Generator.Auth.Provider

spec_AuthProvider :: Spec
spec_AuthProvider = do
  describe "allOAuthProviders" $ do
    it "covers every ExternalAuthConfig field on AuthMethods" $ do
      let allFieldsSet =
            AS.Auth.AuthMethods
              { AS.Auth.usernameAndPassword = Nothing,
                AS.Auth.discord = Just dummyExternalConfig,
                AS.Auth.slack = Just dummyExternalConfig,
                AS.Auth.google = Just dummyExternalConfig,
                AS.Auth.gitHub = Just dummyExternalConfig,
                AS.Auth.keycloak = Just dummyExternalConfig,
                AS.Auth.microsoft = Just dummyExternalConfig,
                AS.Auth.email = Nothing
              }
      let extractedConfigs = map (\spec -> extractConfig spec allFieldsSet) allOAuthProviders
      all isJust extractedConfigs `shouldBe` True

    it "contains 6 providers" $ do
      length allOAuthProviders `shouldBe` 6

    it "has unique slugs" $ do
      let slugs = map slug allOAuthProviders
      length slugs `shouldBe` length (unique slugs)

  describe "enabledOAuthProviders" $ do
    it "returns empty list when no OAuth methods are enabled" $ do
      let enabled = enabledOAuthProviders (makeAuth emptyMethods)
      map (slug . fst) enabled `shouldBe` []

    it "returns only enabled providers" $ do
      let methods =
            emptyMethods
              { AS.Auth.google = Just dummyExternalConfig,
                AS.Auth.slack = Just dummyExternalConfig
              }
      let enabled = enabledOAuthProviders (makeAuth methods)
      length enabled `shouldBe` 2
      map (slug . fst) enabled `shouldMatchList` ["google", "slack"]

    it "returns all providers when all are enabled" $ do
      let methods =
            emptyMethods
              { AS.Auth.google = Just dummyExternalConfig,
                AS.Auth.gitHub = Just dummyExternalConfig,
                AS.Auth.discord = Just dummyExternalConfig,
                AS.Auth.keycloak = Just dummyExternalConfig,
                AS.Auth.slack = Just dummyExternalConfig,
                AS.Auth.microsoft = Just dummyExternalConfig
              }
      let enabled = enabledOAuthProviders (makeAuth methods)
      length enabled `shouldBe` 6

  describe "isOAuthEnabled" $ do
    it "returns False when no OAuth is enabled" $ do
      isOAuthEnabled (makeAuth emptyMethods) `shouldBe` False

    it "returns True when any OAuth is enabled" $ do
      let methods = emptyMethods {AS.Auth.google = Just dummyExternalConfig}
      isOAuthEnabled (makeAuth methods) `shouldBe` True

  describe "isEmailEnabled" $ do
    it "returns False when email is not enabled" $ do
      isEmailEnabled (makeAuth emptyMethods) `shouldBe` False

  describe "isUsernameAndPasswordEnabled" $ do
    it "returns False when username/password is not enabled" $ do
      isUsernameAndPasswordEnabled (makeAuth emptyMethods) `shouldBe` False

    it "returns True when username/password is enabled" $ do
      let methods =
            emptyMethods
              { AS.Auth.usernameAndPassword =
                  Just AS.Auth.UsernameAndPasswordConfig {AS.Auth.userSignupFields = Nothing}
              }
      isUsernameAndPasswordEnabled (makeAuth methods) `shouldBe` True

  describe "enabledAuthMethodsJson" $ do
    it "produces JSON with correct keys for enabled providers" $ do
      let methods = emptyMethods {AS.Auth.google = Just dummyExternalConfig}
      let Object jsonObj = enabledAuthMethodsJson (makeAuth methods)
      KM.lookup "isGoogleAuthEnabled" jsonObj `shouldBe` Just (Bool True)
      KM.lookup "isSlackAuthEnabled" jsonObj `shouldBe` Just (Bool False)
      KM.lookup "isUsernameAndPasswordAuthEnabled" jsonObj `shouldBe` Just (Bool False)
      KM.lookup "isEmailAuthEnabled" jsonObj `shouldBe` Just (Bool False)

    it "uses displayName for JSON keys (GitHub has capital H)" $ do
      let methods = emptyMethods {AS.Auth.gitHub = Just dummyExternalConfig}
      let Object jsonObj = enabledAuthMethodsJson (makeAuth methods)
      KM.lookup "isGitHubAuthEnabled" jsonObj `shouldBe` Just (Bool True)

  describe "serverOAuthLoginUrl" $ do
    it "constructs URL from slug" $ do
      let spec = head allOAuthProviders -- google
      serverOAuthLoginUrl spec `shouldBe` "/auth/google/login"

-- Helpers

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

dummyExternalConfig :: AS.Auth.ExternalAuthConfig
dummyExternalConfig =
  AS.Auth.ExternalAuthConfig
    { AS.Auth.configFn = Nothing,
      AS.Auth.userSignupFields = Nothing
    }

emptyMethods :: AS.Auth.AuthMethods
emptyMethods =
  AS.Auth.AuthMethods
    { AS.Auth.usernameAndPassword = Nothing,
      AS.Auth.discord = Nothing,
      AS.Auth.slack = Nothing,
      AS.Auth.google = Nothing,
      AS.Auth.gitHub = Nothing,
      AS.Auth.keycloak = Nothing,
      AS.Auth.microsoft = Nothing,
      AS.Auth.email = Nothing
    }

makeAuth :: AS.Auth.AuthMethods -> AS.Auth.Auth
makeAuth methods =
  AS.Auth.Auth
    { AS.Auth.userEntity = AS.Core.Ref.Ref "User",
      AS.Auth.externalAuthEntity = Nothing,
      AS.Auth.methods = methods,
      AS.Auth.onAuthFailedRedirectTo = "/",
      AS.Auth.onAuthSucceededRedirectTo = Nothing,
      AS.Auth.onBeforeSignup = Nothing,
      AS.Auth.onAfterSignup = Nothing,
      AS.Auth.onAfterEmailVerified = Nothing,
      AS.Auth.onBeforeOAuthRedirect = Nothing,
      AS.Auth.onBeforeLogin = Nothing,
      AS.Auth.onAfterLogin = Nothing
    }
