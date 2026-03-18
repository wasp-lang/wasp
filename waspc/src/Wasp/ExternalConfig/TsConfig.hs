{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    CompilerOptions (..),
    TsConfigFile,
    parseTsConfigFile,
  )
where

import Control.Arrow (left)
import Data.Aeson
  ( FromJSON,
    genericParseJSON,
    parseJSON,
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS
import GHC.Generics (Generic)
import StrongPath (Abs, File, Path', basename, fromRelFile)
import Wasp.Util (indent)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

data TsConfig = TsConfig
  { compilerOptions :: !CompilerOptions,
    include :: !(Maybe [String])
  }
  deriving (Show, Generic, FromJSON)

data CompilerOptions = CompilerOptions
  { _module :: !(Maybe String),
    target :: !(Maybe String),
    composite :: !(Maybe Bool),
    skipLibCheck :: !(Maybe Bool),
    moduleResolution :: !(Maybe String),
    moduleDetection :: !(Maybe String),
    isolatedModules :: !(Maybe Bool),
    jsx :: !(Maybe String),
    strict :: !(Maybe Bool),
    esModuleInterop :: !(Maybe Bool),
    lib :: !(Maybe [String]),
    allowJs :: !(Maybe Bool),
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

class TsConfigFile f

parseTsConfigFile :: (TsConfigFile f) => Path' Abs (File f) -> IO (Either String TsConfig)
parseTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile
  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent
  return $ left ((errorMessagePrefix ++) . indent 2) parseResult
  where
    errorMessagePrefix = "Failed to parse '" ++ baseTsConfigFilePath ++ "':\n"
    baseTsConfigFilePath = fromRelFile (basename tsConfigFile)
