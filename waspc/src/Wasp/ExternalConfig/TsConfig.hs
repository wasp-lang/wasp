{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    CompilerOptions (..),
    ImportPathMapping (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (..),
    genericParseJSON,
    parseJSON,
    withObject,
  )
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)

data TsConfig = TsConfig
  { compilerOptions :: !CompilerOptions
  }
  deriving (Show, Generic, FromJSON)

data CompilerOptions = CompilerOptions
  { _module :: !(Maybe String),
    target :: !(Maybe String),
    composite :: !(Maybe Bool),
    moduleResolution :: !(Maybe String),
    jsx :: !(Maybe String),
    strict :: !(Maybe Bool),
    esModuleInterop :: !(Maybe Bool),
    lib :: !(Maybe [String]),
    allowJs :: !(Maybe Bool),
    typeRoots :: !(Maybe [String]),
    outDir :: !(Maybe String),
    baseUrl :: !(Maybe String),
    paths :: !(Maybe ImportPathMapping)
  }
  deriving (Show, Generic)

data ImportPathMapping
  = ImportPathMapping (Map String String)
  deriving (Show, Generic, ToJSON)

instance FromJSON ImportPathMapping where
  parseJSON = withObject "PathMappings" $ \object ->
    ImportPathMapping . M.mapWithKey getOnlyLocationOrFail <$> parseJSON (Object object)
    where
      getOnlyLocationOrFail path = \case
        [lookupLocation] -> lookupLocation
        [] -> fail "Found empty lookup array value for path '" ++ path ++ "' in tsconfig.json"
        (_ : _) ->
          fail "Found multiple lookup locations for path '"
            ++ path
            ++ "' in tsconfig.json. Wasp only supports one-to-one path mappings"

instance FromJSON CompilerOptions where
  parseJSON =
    genericParseJSON $
      Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      -- "module" is a reserved keyword in Haskell, so we use "_module" instead.
      modifyFieldLabel "_module" = "module"
      modifyFieldLabel other = other
