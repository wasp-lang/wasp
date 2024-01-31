module Wasp.Generator.SdkGenerator.Server.EmailSenderG where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Data.Text
import StrongPath (File', Path', Rel, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.App.EmailSender (EmailSender)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
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
genIndex email = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|server/email/index.ts|]
    tmplData = getEmailProvidersJson email

genCore :: EmailSender -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      genCoreTypes email,
      genCoreHelpers email
    ]
    <++> genEmailSenderProviderSetupFn email

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex email = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|server/email/core/index.ts|]
    tmplData = getEmailProvidersJson email

genCoreTypes :: EmailSender -> Generator FileDraft
genCoreTypes email = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|server/email/core/types.ts|]
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom email

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers email = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|server/email/core/helpers.ts|]
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

genEmailSenderProviderSetupFn :: EmailSender -> Generator [FileDraft]
genEmailSenderProviderSetupFn email =
  sequence
    [ genFileCopy tmplPath
    ]
  where
    provider :: Providers.EmailSenderProvider
    provider = getEmailSenderProvider email

    tmplPath = Providers.providersDirInSdkTemplatesDir </> Providers.setupFnFile provider

depsRequiredByEmail :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByEmail spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe Providers.EmailSenderProvider
    maybeProvider = getEmailSenderProvider <$> (AS.App.emailSender . snd . getApp $ spec)
    maybeNpmDepedency = maybeProvider >>= Providers.npmDependency

getEmailProvidersJson :: EmailSender -> Aeson.Value
getEmailProvidersJson email =
  object [isEnabledKey .= True]
  where
    provider :: Providers.EmailSenderProvider
    provider = getEmailSenderProvider email
    isEnabledKey = Data.Text.pack $ Providers.isEnabledKey provider

getEmailSenderProvider :: EmailSender -> Providers.EmailSenderProvider
getEmailSenderProvider email = case AS.EmailSender.provider email of
  AS.EmailSender.SMTP -> Providers.smtp
  AS.EmailSender.SendGrid -> Providers.sendGrid
  AS.EmailSender.Mailgun -> Providers.mailgun
  AS.EmailSender.Dummy -> Providers.dummy

genFileCopy :: Path' (Rel C.SdkTemplatesDir) File' -> Generator FileDraft
genFileCopy = return . C.mkTmplFd
