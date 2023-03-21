module Wasp.Generator.ServerGenerator.EmailSenderG where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, maybeToList)
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
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.EmailSender.Providers as Providers
import Wasp.Util ((<++>))

genEmailSender :: AppSpec -> Generator [FileDraft]
genEmailSender spec = case maybeEmailSender of
  Just emailSender ->
    sequence
      [ genIndex emailSender
      ]
      <++> genCore emailSender
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex email = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/email/index.ts|]
    tmplData = getEmailProvidersJson email

genCore :: EmailSender -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      genCoreTypes email,
      genCoreHelpers email,
      copyTmplFile [relfile|email/core/providers/dummy.ts|]
    ]
    <++> genEmailSenderProviderSetupFn email

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex email = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/email/core/index.ts|]
    tmplData = getEmailProvidersJson email

genCoreTypes :: EmailSender -> Generator FileDraft
genCoreTypes email = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/email/core/types.ts|]
    tmplData =
      object ["isDefaultFromFieldDefined" .= isDefaultFromFieldDefined]
    isDefaultFromFieldDefined = isJust defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom email

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers email = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/email/core/helpers.ts|]
    tmplData =
      object
        [ "defaultFromField"
            .= object
              [ "email" .= maybeEmail,
                "name" .= maybeName,
                "isNameDefined" .= isJust maybeName
              ],
          "isDefaultFromFieldDefined" .= isDefaultFromFieldDefined
        ]
    isDefaultFromFieldDefined = isJust defaultFromField
    maybeEmail = AS.EmailSender.email <$> defaultFromField
    maybeName = AS.EmailSender.name <$> defaultFromField
    defaultFromField = AS.EmailSender.defaultFrom email

genEmailSenderProviderSetupFn :: EmailSender -> Generator [FileDraft]
genEmailSenderProviderSetupFn email =
  sequence
    [ copyTmplFile tmplPath
    ]
  where
    provider :: Providers.EmailSenderProvider
    provider = getEmailSenderProvider email

    tmplPath = Providers.providersDirInServerSrc </> Providers.setupFnFile provider

depsRequiredByEmail :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByEmail spec = maybeToList maybeNpmDepedency
  where
    maybeProvider :: Maybe Providers.EmailSenderProvider
    maybeProvider = getEmailSenderProvider <$> (AS.App.emailSender . snd . getApp $ spec)
    maybeNpmDepedency = Providers.npmDependency <$> maybeProvider

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

copyTmplFile :: Path' (Rel C.ServerTemplatesSrcDir) File' -> Generator FileDraft
copyTmplFile = return . C.mkSrcTmplFd
