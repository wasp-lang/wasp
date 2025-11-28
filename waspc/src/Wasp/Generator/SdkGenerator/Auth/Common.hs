module Wasp.Generator.SdkGenerator.Auth.Common where

import Data.Maybe (fromMaybe)
import qualified Wasp.AppSpec.App.Auth as AS.Auth

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)

getOnAuthFailedRedirectTo :: AS.Auth.Auth -> String
getOnAuthFailedRedirectTo = AS.Auth.onAuthFailedRedirectTo
