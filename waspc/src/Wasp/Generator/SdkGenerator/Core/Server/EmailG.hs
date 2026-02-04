{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.Server.EmailG
  ( genServerEmail,
    depsRequiredByEmailSenderProviders,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.EmailSender (EmailSender)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import qualified Wasp.Generator.SdkGenerator.Core.Server.EmailSenderProviders as EmailSenderProviders

genServerEmail :: AppSpec -> Generator [FileDraft]
genServerEmail spec = case maybeEmailSender of
  Just emailSender -> genServerEmailCore emailSender
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genServerEmailCore :: EmailSender -> Generator [FileDraft]
genServerEmailCore emailSender =
  sequence
    [ genServerEmailCoreIndex emailSender,
      genServerEmailCoreTypes emailSender,
      genServerEmailCoreHelpers emailSender,
      genEmailSenderProviderSetupFn emailSender
    ]

genServerEmailCoreIndex :: EmailSender -> Generator FileDraft
genServerEmailCoreIndex emailSender =
  return $ mkTmplFdWithData [relfile|server/email/core/index.ts|] tmplData
  where
    tmplData = EmailSenders.getEnabledEmailProvidersJson emailSender

genServerEmailCoreTypes :: EmailSender -> Generator FileDraft
genServerEmailCoreTypes emailSender =
  return $ mkTmplFdWithData [relfile|server/email/core/types.ts|] tmplData
  where
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom emailSender

genServerEmailCoreHelpers :: EmailSender -> Generator FileDraft
genServerEmailCoreHelpers emailSender =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/email/core/helpers.ts|]
    tmplData =
      object
        [ "defaultFromField"
            .= object
              [ "email" .= fromMaybe "" maybeEmail,
                "name" .= fromMaybe "" maybeName,
                "isNameDefined" .= isJust maybeName
              ],
          "isDefaultFromFieldDefined" .= isDefaultFromFieldDefined
        ]
    isDefaultFromFieldDefined = isJust defaultFromField
    maybeEmail = AS.EmailSender.email <$> defaultFromField
    maybeName = defaultFromField >>= AS.EmailSender.name
    defaultFromField = AS.EmailSender.defaultFrom emailSender

genEmailSenderProviderSetupFn :: EmailSender -> Generator FileDraft
genEmailSenderProviderSetupFn emailSender =
  return . mkTmplFd $ ([reldir|server/email/core/providers|] </> setupFnFromProvidersDir)
  where
    setupFnFromProvidersDir = EmailSenderProviders.setupFnFile . EmailSenderProviders.getEmailSenderProvider $ emailSender

depsRequiredByEmailSenderProviders :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByEmailSenderProviders spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe EmailSenderProviders.EmailSenderProvider
    maybeProvider = EmailSenderProviders.getEmailSenderProvider <$> (snd . getApp $ spec).emailSender
    maybeNpmDepedency = maybeProvider >>= EmailSenderProviders.npmDependency
