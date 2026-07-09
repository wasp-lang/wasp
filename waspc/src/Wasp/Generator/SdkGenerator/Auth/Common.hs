module Wasp.Generator.SdkGenerator.Auth.Common where

import Data.Maybe (fromMaybe)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Destination as AS.Destination

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> AS.Destination.Destination
getOnAuthSucceededRedirectToOrDefault auth =
  fromMaybe defaultOnAuthSucceededRedirectTo (AS.Auth.onAuthSucceededRedirectTo auth)
  where
    defaultOnAuthSucceededRedirectTo =
      AS.Destination.Destination
        { AS.Destination.kind = AS.Destination.Route,
          AS.Destination.path = "/"
        }
