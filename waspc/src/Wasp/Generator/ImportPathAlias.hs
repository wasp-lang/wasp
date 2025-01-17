module Wasp.Generator.ImportPathAlias
  ( createTsConfigPathsTemplateData,
    createRollupAliasesTemplateData,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified StrongPath as SP
import System.FilePath ((</>))
import Wasp.ExternalConfig.TsConfig (ImportPathMapping (..))
import Wasp.Project.Common (waspProjectDirFromSrcDir)
import Wasp.Util (stripSuffixIfPresent)

createTsConfigPathsTemplateData :: Maybe ImportPathMapping -> [Aeson.Value]
createTsConfigPathsTemplateData = \case
  Nothing -> []
  Just (ImportPathMapping mappings) -> map makePathObjectEntry $ M.toList mappings
  where
    makePathObjectEntry (path, lookupLocation) =
      Aeson.object
        [ "path" .= path,
          "lookupLocation" .= lookupLocation
        ]

createRollupAliasesTemplateData :: Maybe ImportPathMapping -> [Aeson.Value]
createRollupAliasesTemplateData = \case
  Nothing -> []
  Just (ImportPathMapping mappings) -> map makeAliasTemplateData $ M.toList mappings
  where
    makeAliasTemplateData (path, lookupLocation) =
      Aeson.object
        [ "find" .= stripWildcardPath path,
          "replacement" .= (SP.fromRelDir waspProjectDirFromSrcDir </> stripWildcardPath lookupLocation)
        ]
    stripWildcardPath = stripSuffixIfPresent "/*"
