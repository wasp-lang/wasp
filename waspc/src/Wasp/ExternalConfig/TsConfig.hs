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
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
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
  parseJSON = withObject "PathMappings" $ \object -> do
    originalMappings <- parseJSON (Object object)

    let nonEmptyMappings = M.mapMaybe nonEmpty originalMappings
    let (invalidMappings, validMappings) = M.mapEither getOnlyLookupLocationOrError nonEmptyMappings

    case M.toList invalidMappings of
      [] -> return $ ImportPathMapping validMappings
      invalid -> fail $ makeMultipleLocationsErrorMsg invalid
    where
      getOnlyLookupLocationOrError :: NonEmpty String -> Either (NonEmpty String) String
      getOnlyLookupLocationOrError = \case
        lookupLocation :| [] -> Right lookupLocation
        locations@(_ :| _) -> Left locations

      makeMultipleLocationsErrorMsg :: [(String, NonEmpty String)] -> String
      makeMultipleLocationsErrorMsg locations =
        "One or more paths point to no multiple lookup locations: "
          ++ show ((fmap . fmap) toList locations)
          ++ ". Wasp only supports one-on-one path mappings."

instance FromJSON CompilerOptions where
  parseJSON =
    genericParseJSON $
      Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      -- "module" is a reserved keyword in Haskell, so we use "_module" instead.
      modifyFieldLabel "_module" = "module"
      modifyFieldLabel other = other
