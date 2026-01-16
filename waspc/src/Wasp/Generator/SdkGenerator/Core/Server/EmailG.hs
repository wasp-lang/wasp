{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.Server.EmailG
  ( genServerEmail,
    depsRequiredByEmailSenderProviders,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.EmailSender (EmailSender)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (CoreTemplatesDir, mkTmplFd, mkTmplFdWithData)
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
      genServerEmailCoreProviderSetupFn emailSender
    ]

genServerEmailCoreIndex :: EmailSender -> Generator FileDraft
genServerEmailCoreIndex emailSender =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = serverEmailDirInCoreTemplatesDir </> [relfile|core/index.ts|]
    tmplData = EmailSenders.getEnabledEmailProvidersJson emailSender

genServerEmailCoreTypes :: EmailSender -> Generator FileDraft
genServerEmailCoreTypes emailSender =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = serverEmailDirInCoreTemplatesDir </> [relfile|core/types.ts|]
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom emailSender

genServerEmailCoreHelpers :: EmailSender -> Generator FileDraft
genServerEmailCoreHelpers emailSender =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = serverEmailDirInCoreTemplatesDir </> [relfile|core/helpers.ts|]
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

genServerEmailCoreProviderSetupFn :: EmailSender -> Generator FileDraft
genServerEmailCoreProviderSetupFn emailSender =
  return $ mkTmplFd tmplFile
  where
    tmplFile = EmailSenderProviders.setupFnFile . getEmailSenderProvider $ emailSender

depsRequiredByEmailSenderProviders :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByEmailSenderProviders spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe EmailSenderProviders.EmailSenderProvider
    maybeProvider = getEmailSenderProvider <$> (snd . getApp $ spec).emailSender
    maybeNpmDepedency = maybeProvider >>= EmailSenderProviders.npmDependency

getEmailSenderProvider :: EmailSender -> EmailSenderProviders.EmailSenderProvider
getEmailSenderProvider email = case AS.EmailSender.provider email of
  AS.EmailSender.SMTP -> EmailSenderProviders.smtp
  AS.EmailSender.SendGrid -> EmailSenderProviders.sendGrid
  AS.EmailSender.Mailgun -> EmailSenderProviders.mailgun
  AS.EmailSender.Dummy -> EmailSenderProviders.dummy

serverEmailDirInCoreTemplatesDir :: Path' (Rel CoreTemplatesDir) Dir'
serverEmailDirInCoreTemplatesDir = [reldir|server/email|]
