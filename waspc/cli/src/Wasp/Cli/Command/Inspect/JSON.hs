module Wasp.Cli.Command.Inspect.JSON
  ( inspectAsJson,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Version (waspVersion)

inspectAsJson :: AppSpec -> BSL.ByteString
inspectAsJson appSpec =
  encodePretty $
    object
      [ "waspVersion" .= show waspVersion,
        "decls" .= AS.decls appSpec
      ]
