module Wasp.Generator.SdkGenerator.Auth.SchemeSideConfig
  ( mkSchemeSideConfigTmplData,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.SdkGenerator.JsImport (extImportToAliasedImportJson)

-- | Template data for one half's @config@, shared by the server and client
-- registries.
--
-- A handler's config is one object mixing plain data with references to app
-- code. A reference cannot cross the compiler as data, so the spec mapper
-- lifted each one out, keyed by the path it sat at. The generated code does
-- the inverse: it splices the data in as a literal, imports every reference,
-- and sets each back at its path before calling the handler's factory.
--
-- The alias prefix keeps imports from colliding when several schemes' user
-- modules share an export name.
mkSchemeSideConfigTmplData :: String -> AS.Auth.AuthSchemeSide -> [Aeson.Types.Pair]
mkSchemeSideConfigTmplData aliasPrefix side =
  [ -- Spliced in verbatim: the mapper already proved the text is valid JSON.
    "configJson" .= fromMaybe "undefined" side.configJson,
    "configReferences"
      .= [ object
             [ -- A JSON array of path segments, which is also a valid JS
               -- array literal, so it too is spliced in verbatim.
               "pathJs" .= pathJson,
               "import" .= extImportToAliasedImportJson (aliasPrefix ++ "_" ++ show referenceIdx) (Just extImport)
             ]
         | (referenceIdx, (pathJson, extImport)) <- zip [0 :: Int ..] (Map.toList side.configReferences)
         ]
  ]
