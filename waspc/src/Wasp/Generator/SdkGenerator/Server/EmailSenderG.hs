module Wasp.Generator.SdkGenerator.Server.EmailSenderG
  ( genNewEmailSenderApi,
    depsRequiredByEmail,
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
import Wasp.Generator.SdkGenerator.Common
  ( SdkProject (..),
    SdkTemplatesProjectDir,
    makeSdkProjectTmplFd,
    makeSdkProjectTmplFdWithData,
  )
import qualified Wasp.Generator.SdkGenerator.EmailSender.Providers as Providers
import Wasp.Generator.SdkGenerator.Server.Common (serverTemplatesDirInSdkTemplatesDir)
import Wasp.Util ((<++>))

genNewEmailSenderApi :: AppSpec -> Generator [FileDraft]
genNewEmailSenderApi spec = case maybeEmailSender of
  Just emailSender ->
    sequence
      [ genIndex emailSender
      ]
      <++> genCore emailSender
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex email =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverEmailDirInSdkTemplatesProjectDir </> [relfile|index.ts|]
    tmplData = EmailSenders.getEnabledEmailProvidersJson email

genCore :: EmailSender -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      genCoreTypes email,
      genCoreHelpers email,
      genEmailSenderProviderSetupFn email
    ]

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex email =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverEmailDirInSdkTemplatesProjectDir </> [relfile|core/index.ts|]
    tmplData = EmailSenders.getEnabledEmailProvidersJson email

genCoreTypes :: EmailSender -> Generator FileDraft
genCoreTypes email =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = [relfile|server/email/core/types.ts|]
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom email

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers email =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverEmailDirInSdkTemplatesProjectDir </> [relfile|core/helpers.ts|]
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
    defaultFromField = AS.EmailSender.defaultFrom email

genEmailSenderProviderSetupFn :: EmailSender -> Generator FileDraft
genEmailSenderProviderSetupFn email =
  return $ makeSdkProjectTmplFd SdkUserCoreProject tmplFile
  where
    tmplFile = Providers.serverProvidersDirInSdkTemplatesDir </> Providers.setupFnFile emailSenderProvider
    emailSenderProvider = getEmailSenderProvider email

depsRequiredByEmail :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByEmail spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe Providers.EmailSenderProvider
    maybeProvider = getEmailSenderProvider <$> (AS.App.emailSender . snd . getApp $ spec)
    maybeNpmDepedency = maybeProvider >>= Providers.npmDependency

getEmailSenderProvider :: EmailSender -> Providers.EmailSenderProvider
getEmailSenderProvider email = case AS.EmailSender.provider email of
  AS.EmailSender.SMTP -> Providers.smtp
  AS.EmailSender.SendGrid -> Providers.sendGrid
  AS.EmailSender.Mailgun -> Providers.mailgun
  AS.EmailSender.Dummy -> Providers.dummy

serverEmailDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
serverEmailDirInSdkTemplatesProjectDir = serverTemplatesDirInSdkTemplatesDir </> [reldir|email|]
