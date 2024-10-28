{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.TypeScript.Inspect.Exports
  ( -- * Getting Information About TypeScript Files

    -- Internally, this module calls out to @packages/ts-inspect@, which uses
    -- the TypeScript compiler API.
    --
    -- Despite all of the names and descriptions referring to just TypeScript,
    -- this module also supports JavaScript files.

    -- * Export lists
    getExportsOfTsFiles,
    TsExportsRequest (..),
    TsExportsResponse (..),
    TsExport (..),
    tsExportSourceRegion,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value, decode, defaultOptions, encode, genericToEncoding, withObject, (.:), (.:!))
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Conduit.Process.Typed (ExitCode (ExitSuccess))
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import qualified System.Process as P
import Wasp.Analyzer (SourcePosition)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion))
import Wasp.NodePackageFFI (RunnablePackage (TsInspectPackage), getPackageProcessOptions)

-- | Attempt to get list of exported names from TypeScript files.
--
-- The 'FilePath's in the response are guaranteed to exactly match the
-- corresponding 'FilePath' in the request.
getExportsOfTsFiles :: [TsExportsRequest] -> IO (Either String TsExportsResponse)
getExportsOfTsFiles requests = do
  let requestJSON = BS.toString $ encode $ groupExportRequestsByTsconfig requests
  cp <- getPackageProcessOptions TsInspectPackage []
  (exitCode, response, err) <- P.readCreateProcessWithExitCode cp requestJSON
  case exitCode of
    ExitSuccess -> case decode $ BS.fromString response of
      Nothing -> return $ Left $ "invalid response JSON from ts-inspect: " ++ response
      Just exports -> return $ Right exports
    _ -> return $ Left err

-- | Join exports requests that have the same tsconfig. The @ts-inspect@ package
-- runs an instance of the TypeScript compiler per request group, so grouping
-- them this way improves performance.
groupExportRequestsByTsconfig :: [TsExportsRequest] -> [TsExportsRequest]
groupExportRequestsByTsconfig requests =
  map (uncurry $ flip TsExportsRequest) $
    M.toList $ foldr insertRequest M.empty requests
  where
    insertRequest (TsExportsRequest names maybeTsconfig) grouped =
      M.insertWith (++) maybeTsconfig names grouped

-- | A symbol exported from a TypeScript file.
data TsExport
  = -- | @export default ...@
    DefaultExport !(Maybe SourceRegion)
  | -- | @export const name ...@
    NamedExport !String !(Maybe SourceRegion)
  deriving (Show, Eq)

-- | Get the position of an export in the TypeScript file, if that information
-- is available.
tsExportSourceRegion :: TsExport -> Maybe SourceRegion
tsExportSourceRegion (DefaultExport sourceRegion) = sourceRegion
tsExportSourceRegion (NamedExport _ sourceRegion) = sourceRegion

instance FromJSON TsExport where
  -- The JSON response gives zero-based source positions. This parser takes care
  -- of converting to the expected one-based positions for 'SourcePosition'.
  parseJSON = withObject "TsExport" $ \v ->
    (v .: "type") >>= \case
      "default" -> DefaultExport . fmap toSourceRegion <$> v .:! "range"
      "named" -> NamedExport <$> v .: "name" <*> (fmap toSourceRegion <$> v .:! "range")
      (_ :: Value) -> fail "invalid type for TsExport"

-- | Map from TypeScript files to the list of exports found in that file.
newtype TsExportsResponse = TsExportsResponse (M.HashMap FilePath [TsExport])
  deriving (Eq, Show, FromJSON)

-- | A list of files associated with an optional tsconfig file that is run
-- through the TypeScript compiler as a group.
data TsExportsRequest = TsExportsRequest {filepaths :: ![FilePath], tsconfig :: !(Maybe FilePath)}
  deriving (Eq, Show, Generic)

instance ToJSON TsExportsRequest where
  toEncoding = genericToEncoding defaultOptions

-- Wrapper types for parsing SourceRegions from data with 0-based offsets.

newtype ZeroBasedSourceRegion = ZeroBasedSourceRegion {toSourceRegion :: SourceRegion}

instance FromJSON ZeroBasedSourceRegion where
  parseJSON = withObject "range" $ \v ->
    ZeroBasedSourceRegion
      <$> ( SourceRegion
              <$> (toSourcePos <$> v .: "start")
              <*> (toSourcePos <$> v .: "end")
          )

newtype ZeroBasedSourcePosition = ZeroBasedSourcePosition {toSourcePos :: SourcePosition}

instance FromJSON ZeroBasedSourcePosition where
  parseJSON = withObject "location" $ \v ->
    ZeroBasedSourcePosition
      <$> ( SourcePosition
              <$> ((+ 1) <$> v .: "line")
              <*> ((+ 1) <$> v .: "column")
          )
