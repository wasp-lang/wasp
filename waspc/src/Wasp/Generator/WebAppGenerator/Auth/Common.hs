module Wasp.Generator.WebAppGenerator.Auth.Common where

import Data.Maybe (fromMaybe)
import qualified Wasp.AppSpec.App.Auth as AS.Auth

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)
