module Wasp.Generator.WebAppGenerator.ExternalAuthG
  ( googleAuthInfo,
    gitHubAuthInfo,
    frontendLoginUrl,
    serverLoginUrl,
    serverOauthRedirectHandlerUrl,
    templateFilePathInPassportDir,
    ExternalAuthInfo (..),
  )
where

import StrongPath (File', Path', Rel, Rel', relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ServerGenerator.Common (ServerTemplatesSrcDir)

data ExternalAuthInfo = ExternalAuthInfo
  { _passportTemplateFilePath :: Path' (Rel ServerTemplatesSrcDir) File',
    _logoFileName :: Path' Rel' File',
    _displayName :: String,
    _slug :: String
  }

googleAuthInfo :: ExternalAuthInfo
googleAuthInfo =
  ExternalAuthInfo
    { _passportTemplateFilePath = [relfile|routes/auth/passport/google/config.js|],
      _logoFileName = [relfile|google-logo-icon.png|],
      _displayName = "Google",
      _slug = "google"
    }

gitHubAuthInfo :: ExternalAuthInfo
gitHubAuthInfo =
  ExternalAuthInfo
    { _passportTemplateFilePath = [relfile|routes/auth/passport/github/config.js|],
      _logoFileName = [relfile|github-logo-icon.png|],
      _displayName = "GitHub",
      _slug = "github"
    }

frontendLoginUrl :: ExternalAuthInfo -> String
frontendLoginUrl eai = "/auth/login/" ++ _slug eai

serverLoginUrl :: ExternalAuthInfo -> String
serverLoginUrl eai = "/auth/external/" ++ _slug eai ++ "/login"

serverOauthRedirectHandlerUrl :: ExternalAuthInfo -> String
serverOauthRedirectHandlerUrl eai = "/auth/external/" ++ _slug eai ++ "/validateCodeForLogin"

templateFilePathInPassportDir :: ExternalAuthInfo -> Path' Rel' File'
templateFilePathInPassportDir eai =
  (SP.basename . SP.parent $ _passportTemplateFilePath eai)
    </> SP.basename (_passportTemplateFilePath eai)
