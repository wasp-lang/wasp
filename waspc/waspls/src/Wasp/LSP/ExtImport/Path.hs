{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.LSP.ExtImport.Path
  ( ExtFileCachePath,
    cachePathFile,
    cachePathExtType,
    WaspStyleExtFilePath (WaspStyleExtFilePath),
    waspStylePathToCachePath,
    absPathToCachePath,
    cachePathToAbsPathWithoutExt,
    cachePathToAbsPath,
    ExtensionType,
    allowedExts,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (stripPrefix)
import GHC.Generics (Generic)
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.LSP.ServerMonads.HasProjectRootDir (HasProjectRootDir (getProjectRootDir))
import Wasp.Project.Common (WaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Util.IO (doesFileExist)
import qualified Wasp.Util.StrongPath as SP

data ExtensionlessExtFile

-- | Path used in the exports cache for external code files (JS and TS files).
--
-- It is stored relative to @src/@ and without an extension so that cache lookups
-- (starting with a 'WaspStyleExtFilePath') are more efficient.
data ExtFileCachePath = ExtFileCachePath
  { cachePathFile :: !(SP.Path' (SP.Rel SourceExternalCodeDir) (SP.File ExtensionlessExtFile)),
    cachePathExtType :: !ExtensionType
  }
  deriving (Show, Eq, Generic)

-- | Hashes only the path portion (ignoring the extension type). This is so that
-- hashing does not get in the way of equality comparisons while looking up
-- 'ExtFileCachePath's in a "Data.HashMap".
instance Hashable ExtFileCachePath where
  hashWithSalt salt (ExtFileCachePath cachePath _) = hashWithSalt salt cachePath

-- | A path that would appear in an external import, exactly as it is written in
-- Wasp source code.
--
-- For example, @\"\@src/queries.js\"@ is a wasp style path, but @\"src\/queries.js\"@
-- is not.
newtype WaspStyleExtFilePath = WaspStyleExtFilePath String deriving (Show, Eq)

waspStylePathToCachePath :: WaspStyleExtFilePath -> Maybe ExtFileCachePath
waspStylePathToCachePath (WaspStyleExtFilePath waspStylePath) =
  case stripPrefix "@src/" waspStylePath >>= SP.parseRelFile of
    Nothing -> Nothing
    Just relPath ->
      -- TODO(martin): This code below might need more rethinking with the new restructuring.
      Just $ case SP.splitRelExtension relPath of
        Nothing -> ExtFileCachePath relPath DotAnyTS
        Just (relPathWithoutExt, ext) ->
          ExtFileCachePath relPathWithoutExt (widenExtension ext)

absPathToCachePath :: (HasProjectRootDir m) => SP.Path' SP.Abs (SP.File a) -> m (Maybe ExtFileCachePath)
absPathToCachePath absFile = do
  -- Makes the path relative to src/ and deletes the extension.
  maybeProjectDir <- getProjectRootDir
  case maybeProjectDir of
    Nothing -> pure Nothing
    Just (projectRootDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)) ->
      let srcDir = projectRootDir SP.</> srcDirInWaspProjectDir
       in case SP.stripProperPrefix srcDir absFile of
            Nothing -> pure Nothing
            Just relFile -> case SP.splitRelExtension relFile of
              Nothing -> pure Nothing
              Just (fileWithoutExt, ext) -> pure $ Just $ ExtFileCachePath fileWithoutExt (DotExact ext)

cachePathToAbsPathWithoutExt :: (HasProjectRootDir m) => ExtFileCachePath -> m (Maybe (SP.Path' SP.Abs (SP.File ExtensionlessExtFile)))
cachePathToAbsPathWithoutExt (ExtFileCachePath cachePath _) = do
  -- Converts to an absolute path, but does not add any extension.
  maybeProjectDir <- getProjectRootDir
  case maybeProjectDir of
    Nothing -> return Nothing
    Just (projectRootDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)) -> do
      return $ Just $ projectRootDir SP.</> srcDirInWaspProjectDir SP.</> cachePath

cachePathToAbsPath :: forall m a. (MonadIO m, HasProjectRootDir m) => ExtFileCachePath -> m (Maybe (SP.Path' SP.Abs (SP.File a)))
cachePathToAbsPath cp@(ExtFileCachePath _ extType) =
  cachePathToAbsPathWithoutExt cp >>= \case
    Nothing -> return Nothing
    Just absPathWithoutExt -> useFirstExtensionThatExists absPathWithoutExt $ allowedExts extType
  where
    useFirstExtensionThatExists :: SP.Path' SP.Abs (SP.File ExtensionlessExtFile) -> [String] -> m (Maybe (SP.Path' SP.Abs (SP.File a)))
    useFirstExtensionThatExists _ [] = pure Nothing
    useFirstExtensionThatExists file (ext : exts) =
      case P.addExtension ext (SP.toPathAbsFile file) of
        Nothing -> error "Invalid extension in return from 'allowedExts'"
        Just fileWithExt -> do
          fileWithExtExists <- liftIO $ doesFileExist $ SP.fromPathAbsFile fileWithExt
          if fileWithExtExists
            then pure $ Just $ SP.fromPathAbsFile fileWithExt
            else useFirstExtensionThatExists file exts

-- | The \"type\" of an extension. This type is related to how TypeScript resolves
-- module extensions.
data ExtensionType
  = -- | @.ts@ or @.js@.
    DotTJS
  | -- | @.tsx@ or @.jsx@.
    DotTJSX
  | -- | @.ts@, @.js@, @.tsx@, or @.jsx@.
    DotAnyTS
  | -- | @DotExact ext@ is exactly the extension @ext@.
    DotExact !String
  deriving (Show)

-- | Here, widening an extension means @.js@ and @.jsx@ extensions get converted
-- to 'DotTJS' and 'DotTJSX', respectively, meaning they will compare equal to
-- @.ts@ and @.tsx@ extensions.
widenExtension :: String -> ExtensionType
widenExtension ".js" = DotTJS
widenExtension ".jsx" = DotTJSX
widenExtension ext = DotExact ext

-- | The extensions that each 'ExtensionType' represents.
allowedExts :: ExtensionType -> [String]
allowedExts DotTJS = [".ts", ".js"]
allowedExts DotTJSX = [".tsx", ".jsx"]
allowedExts DotAnyTS = [".ts", ".js", ".tsx", ".jsx"]
allowedExts (DotExact ext) = [ext]

-- | The 'Eq' instance on this type is slightly weird: it represents compatibilty,
-- not equality. Specifically, two extension types are equal if they share at
-- least one common extension. This instance is written this way so that the
-- "Data.Hashmap" using this as a key (the exports cache) behaves properly when
-- extension types are different between the key in the cache and the key in the
-- lookup request.
instance Eq ExtensionType where
  DotTJS == DotTJS = True
  DotTJSX == DotTJSX = True
  DotAnyTS == DotTJS = True
  DotAnyTS == DotTJSX = True
  DotAnyTS == DotAnyTS = True
  DotExact ext == right = ext `elem` allowedExts right
  left == right = right == left
