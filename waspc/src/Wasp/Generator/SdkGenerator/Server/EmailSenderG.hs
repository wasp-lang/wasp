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
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import qualified Wasp.Generator.SdkGenerator.EmailSender.Providers as Providers
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
genIndex emailSender =
  return $
    mkTmplFdWithData
      (serverEmailDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData = EmailSenders.getEnabledEmailProvidersJson emailSender

genCore :: EmailSender -> Generator [FileDraft]
genCore emailSender =
  sequence
    [ genCoreIndex emailSender,
      genCoreTypes emailSender,
      genCoreHelpers emailSender,
      genEmailSenderProviderSetupFn emailSender
    ]

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex emailSender =
  return $
    mkTmplFdWithData
      (serverEmailDirInSdkTemplatesDir </> [relfile|core/index.ts|])
      tmplData
  where
    tmplData = EmailSenders.getEnabledEmailProvidersJson emailSender

genCoreTypes :: EmailSender -> Generator FileDraft
genCoreTypes emailSender =
  return $
    mkTmplFdWithData
      (serverEmailDirInSdkTemplatesDir </> [relfile|core/types.ts|])
      tmplData
  where
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom emailSender

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers emailSender =
  return $
    mkTmplFdWithData
      (serverEmailDirInSdkTemplatesDir </> [relfile|core/helpers.ts|])
      tmplData
  where
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

genEmailSenderProviderSetupFn :: AS.EmailSender.EmailSender -> Generator FileDraft
genEmailSenderProviderSetupFn emailSender =
  genFileCopy ([reldir|server/email/core/providers|] </> setupFnFromProvidersDir)
  where
    setupFnFromProvidersDir = Providers.setupFnFile . Providers.getEmailSenderProvider $ emailSender

depsRequiredByEmail :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByEmail spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe Providers.EmailSenderProvider
    maybeProvider = Providers.getEmailSenderProvider <$> (AS.App.emailSender . snd . getApp $ spec)
    maybeNpmDepedency = maybeProvider >>= Providers.npmDependency

serverEmailDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverEmailDirInSdkTemplatesDir = [reldir|server/email|]
