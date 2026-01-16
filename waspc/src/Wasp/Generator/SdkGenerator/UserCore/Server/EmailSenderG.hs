module Wasp.Generator.SdkGenerator.UserCore.Server.EmailSenderG
  ( genNewEmailSenderApi,
  )
where

import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( UserCoreTemplatesDir,
    mkTmplFdWithData,
  )

genNewEmailSenderApi :: AppSpec -> Generator [FileDraft]
genNewEmailSenderApi spec = case maybeEmailSender of
  Just emailSender ->
    sequence [genIndex emailSender]
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex email =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = serverEmailDirInUserCoreTemplatesDir </> [relfile|index.ts|]
    tmplData = EmailSenders.getEnabledEmailProvidersJson email

serverEmailDirInUserCoreTemplatesDir :: Path' (Rel UserCoreTemplatesDir) Dir'
serverEmailDirInUserCoreTemplatesDir = [reldir|server/email|]
