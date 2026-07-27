module Wasp.Generator.SdkGenerator.Auth.Common where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Util (getRoutePathFromRef)

getOnAuthFailedRedirectTo :: AppSpec -> AS.Auth.Auth -> String
getOnAuthFailedRedirectTo spec auth =
  getRoutePathFromRef spec (AS.Auth.onAuthFailedRedirectTo auth)

getOnAuthSucceededRedirectToOrDefault :: AppSpec -> AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault spec auth =
  maybe "/" (getRoutePathFromRef spec) (AS.Auth.onAuthSucceededRedirectTo auth)
