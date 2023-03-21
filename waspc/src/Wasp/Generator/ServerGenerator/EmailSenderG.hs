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
genIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/index.ts|]
    tmplData = getEmailProvidersJson email

genCore :: EmailSender -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      copyTmplFile [relfile|email/core/types.ts|],
      genCoreHelpers email,
      copyTmplFile [relfile|email/core/providers/dummy.ts|]
    ]
    <++> genEmailSenderProviderSetupFn email

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/index.ts|]
    tmplData = getEmailProvidersJson email

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/helpers.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/helpers.ts|]
    tmplData =
      object
        [ "senderDefaults"
            .= object
              [ "email" .= AS.EmailSender.email emailFrom,
                "name" .= name,
                "isNameDefined" .= isJust name
              ]
        ]
    emailFrom = AS.EmailSender.defaultFrom email
    name = AS.EmailSender.name emailFrom

genEmailSenderProviderSetupFn :: EmailSender -> Generator [FileDraft]
genEmailSenderProviderSetupFn email =
  sequence
    [ copyTmplFile $ Providers.setupFnPath provider
    ]
  where
    provider :: Providers.EmailSenderProvider
    provider = getEmailSenderProvider email

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