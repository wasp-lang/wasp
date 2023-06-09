{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.TypeScript
  ( getExportsOfTsFile,
    TsExport,
  )
where

import Control.Concurrent (newChan, readChan, writeChan)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value, decode, defaultOptions, encode, genericToEncoding, withObject, (.:), (.:?))
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Wasp.Analyzer (SourcePosition)
import Wasp.Package (Package (TsInspectPackage), runPackageAsJob)

getExportsOfTsFile :: [TsExportRequest] -> IO (Maybe TsExportResponse)
getExportsOfTsFile requests = do
  let requestJSON = encode requests
  chan <- newChan
  exportsJob <- runPackageAsJob TsInspectPackage chan
  writeChan chan requestJSON
  responseJSON <- readChan chan
  return $ decode responseJSON

data TsExport
  = DefaultExport !(Maybe SourcePosition)
  | NamedExport !String !(Maybe SourcePosition)
  deriving (Show, Eq)

instance FromJSON TsExport where
  parseJSON = withObject "TsExport" $ \v ->
    (v .: "type") >>= \case
      "default" -> DefaultExport <$> v .:? "location"
      "named" -> NamedExport <$> v .: "name" <*> v .:? "location"
      (_ :: Value) -> fail "invalid type for TsExport"

newtype TsExportResponse = TsExportResponse (M.HashMap FilePath [TsExport])
  deriving (Eq, Show, FromJSON)

data TsExportRequest = TsExportRequest {filenames :: ![FilePath], tsconfig :: !(Maybe FilePath)}
  deriving (Eq, Show, Generic)

instance ToJSON TsExportRequest where
  toEncoding = genericToEncoding defaultOptions
