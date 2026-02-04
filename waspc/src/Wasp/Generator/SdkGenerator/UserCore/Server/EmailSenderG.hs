module Wasp.Generator.SdkGenerator.UserCore.Server.EmailSenderG
  ( genNewEmailSenderApi,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFdWithData,
  )

genNewEmailSenderApi :: AppSpec -> Generator [FileDraft]
genNewEmailSenderApi spec = case maybeEmailSender of
  Just emailSender ->
    sequence [genIndex emailSender]
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex emailSender =
  return $ mkTmplFdWithData [relfile|server/email/index.ts|] tmplData
  where
    tmplData = EmailSenders.getEnabledEmailProvidersJson emailSender
