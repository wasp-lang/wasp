{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    CompilerOptions (..),
  )
where

import Data.Aeson
  ( FromJSON,
    genericParseJSON,
    parseJSON,
  )
import qualified Data.Aeson as Aeson
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
    outDir :: !(Maybe String)
  }
  deriving (Show, Generic)

instance FromJSON CompilerOptions where
  parseJSON =
    genericParseJSON $
      Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      -- "module" is a reserved keyword in Haskell, so we use "_module" instead.
      modifyFieldLabel "_module" = "module"
      modifyFieldLabel other = other
