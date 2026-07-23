module Wasp.Cli.Command.Inspect.JSON
  ( inspectAsJson,
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Version (waspVersion)

inspectAsJson :: AppSpec -> String
inspectAsJson appSpec =
  BSL8.unpack $ encodePretty outputObject <> "\n"
  where
    outputObject =
      object
        [ "waspVersion" .= show waspVersion,
          "decls" .= AS.decls appSpec
        ]
