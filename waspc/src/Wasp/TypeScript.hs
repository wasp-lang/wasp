{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.TypeScript
  ( getExportsOfTsFiles,
    TsExportRequest (..),
    TsExportResponse (..),
    TsExport (..),
    tsExportSourcePos,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value, decode, defaultOptions, encode, genericToEncoding, withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Conduit.Process.Typed (ExitCode (ExitSuccess))
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import qualified System.Process as P
import Wasp.Analyzer (SourcePosition)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
import Wasp.Package (Package (TsInspectPackage), getPackageProc)

-- TODO(before merge): add doc comments to this file

getExportsOfTsFiles :: [TsExportRequest] -> IO (Either String TsExportResponse)
getExportsOfTsFiles requests = do
  let requestJSON = BS.toString $ encode $ groupExportRequests requests
  cp <- getPackageProc TsInspectPackage []
  (exitCode, response, err) <- P.readCreateProcessWithExitCode cp requestJSON
  case exitCode of
    ExitSuccess -> case decode $ BS.fromString response of
      Nothing -> return $ Left $ "invalid response JSON from ts-inspect: " ++ response
      Just exports -> return $ Right exports
    _ -> return $ Left err

-- | Join export requests that have the same tsconfig.
groupExportRequests :: [TsExportRequest] -> [TsExportRequest]
groupExportRequests requests =
  map (uncurry $ flip TsExportRequest) $
    M.toList $ foldr insertRequest M.empty requests
  where
    insertRequest (TsExportRequest names maybeTsconfig) grouped =
      M.insertWith (++) maybeTsconfig names grouped

data TsExport
  = DefaultExport !(Maybe SourcePosition)
  | NamedExport !String !(Maybe SourcePosition)
  deriving (Show, Eq)

tsExportSourcePos :: TsExport -> Maybe SourcePosition
tsExportSourcePos (DefaultExport sourcePos) = sourcePos
tsExportSourcePos (NamedExport _ sourcePos) = sourcePos

instance FromJSON TsExport where
  -- The JSON response gives zero-based source positions. This parser takes care
  -- of converting to the expected one-based positions for 'SourcePosition'.
  parseJSON = withObject "TsExport" $ \v ->
    (v .: "type") >>= \case
      "default" -> DefaultExport . fmap toSourcePos <$> v .:? "location"
      "named" -> NamedExport <$> v .: "name" <*> (fmap toSourcePos <$> v .:? "location")
      (_ :: Value) -> fail "invalid type for TsExport"

newtype ZeroBasedSourcePosition = ZeroBasedSourcePosition {toSourcePos :: SourcePosition}

instance FromJSON ZeroBasedSourcePosition where
  parseJSON = withObject "location" $ \v ->
    ZeroBasedSourcePosition
      <$> ( SourcePosition
              <$> ((+ 1) <$> v .: "line")
              <*> ((+ 1) <$> v .: "column")
          )

newtype TsExportResponse = TsExportResponse (M.HashMap FilePath [TsExport])
  deriving (Eq, Show, FromJSON)

data TsExportRequest = TsExportRequest {filenames :: ![FilePath], tsconfig :: !(Maybe FilePath)}
  deriving (Eq, Show, Generic)

instance ToJSON TsExportRequest where
  toEncoding = genericToEncoding defaultOptions
