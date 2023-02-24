module Wasp.Generator.AuthProviders.OAuth
  ( mkExternalAuthInfo,
    frontendLoginUrl,
    serverLoginUrl,
    serverOauthRedirectHandlerUrl,
    templateFilePathInPassportDir,
    slug,
    passportTemplateFilePath,
    displayName,
    logoFileName,
    passportDependency,
    ExternalAuthInfo,
  )
where

import StrongPath (File', Path', Rel, Rel', (</>))
import qualified StrongPath as SP
import Wasp.AppSpec.App.Dependency (Dependency)
import Wasp.Generator.ServerGenerator.Common (ServerTemplatesSrcDir)

data ExternalAuthInfo = ExternalAuthInfo
  { _slug :: String,
    _displayName :: String,
    _passportTemplateFilePath :: Path' (Rel ServerTemplatesSrcDir) File',
    _logoFileName :: Path' Rel' File',
    _passportDependency :: Dependency
  }

mkExternalAuthInfo ::
  String ->
  String ->
  Path' (Rel ServerTemplatesSrcDir) File' ->
  Path' Rel' File' ->
  Dependency ->
  ExternalAuthInfo
mkExternalAuthInfo = ExternalAuthInfo

slug :: ExternalAuthInfo -> String
slug = _slug

passportTemplateFilePath :: ExternalAuthInfo -> Path' (Rel ServerTemplatesSrcDir) File'
passportTemplateFilePath = _passportTemplateFilePath

displayName :: ExternalAuthInfo -> String
displayName = _displayName

logoFileName :: ExternalAuthInfo -> Path' Rel' File'
logoFileName = _logoFileName

passportDependency :: ExternalAuthInfo -> Dependency
passportDependency = _passportDependency

frontendLoginUrl :: ExternalAuthInfo -> String
frontendLoginUrl eai = "/auth/login/" ++ _slug eai

serverLoginUrl :: ExternalAuthInfo -> String
serverLoginUrl eai = "/auth/" ++ _slug eai ++ "/login"

serverOauthRedirectHandlerUrl :: ExternalAuthInfo -> String
serverOauthRedirectHandlerUrl eai = "/auth/" ++ _slug eai ++ "/callback"

templateFilePathInPassportDir :: ExternalAuthInfo -> Path' Rel' File'
templateFilePathInPassportDir eai =
  (SP.basename . SP.parent $ _passportTemplateFilePath eai)
    </> SP.basename (_passportTemplateFilePath eai)
