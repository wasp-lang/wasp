module Wasp.Generator.PathAlias where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.List.Extra (stripSuffix)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified StrongPath as SP
import System.FilePath ((</>))
import Wasp.ExternalConfig.TsConfig (PathMappings (..))
import Wasp.Project.Common (waspProjectDirFromSrcDir)
import Wasp.Util.Aeson (encodeToString)

importPathMappingsToTsConfigChunk :: Maybe PathMappings -> String
importPathMappingsToTsConfigChunk = maybe "{}" (encodeToString . listifyMappings)
  where
    listifyMappings (PathMappings mappings) = M.map (: []) mappings

createTsConfigPathsTemplateData :: Maybe PathMappings -> [Aeson.Value]
createTsConfigPathsTemplateData = \case
  Nothing -> []
  Just (PathMappings mappings) -> map createMappingTemplateData $ M.toList mappings
  where
    createMappingTemplateData (path, lookupLocation) =
      Aeson.object
        [ "path" .= path,
          "lookupLocation" .= lookupLocation
        ]

createRollupAliasesTemplateData :: Maybe PathMappings -> [Aeson.Value]
createRollupAliasesTemplateData = \case
  Nothing -> []
  Just (PathMappings mappings) -> map createAliasTemplateData $ M.toList mappings
  where
    createAliasTemplateData (path, lookupLocation) =
      Aeson.object
        [ "find" .= stripWildcardPath path,
          "replacement" .= (SP.fromRelDir waspProjectDirFromSrcDir </> stripWildcardPath lookupLocation)
        ]
    stripWildcardPath = stripSuffixIfPresent "/*"

stripSuffixIfPresent :: Eq a => [a] -> [a] -> [a]
stripSuffixIfPresent suffix str = fromMaybe str $ stripSuffix suffix str
