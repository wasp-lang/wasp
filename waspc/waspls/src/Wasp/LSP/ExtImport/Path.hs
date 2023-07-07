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
    tryGetTsconfigForAbsPath,
    ExtensionType,
    replaceExtension,
    allowedExts,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Language.LSP.Server as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.IO (doesFileExist)

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
-- For example, @\"\@server/queries.js\"@ is a wasp style path, but @\"src\/server\/queries.js\"@
-- is not.
newtype WaspStyleExtFilePath = WaspStyleExtFilePath String deriving (Show, Eq)

waspStylePathToCachePath :: WaspStyleExtFilePath -> Maybe ExtFileCachePath
waspStylePathToCachePath (WaspStyleExtFilePath waspStylePath) = do
  -- Removes the @, making it a path relative to src/, and remove the extension.
  relFile <- P.parseRelFile =<< stripPrefix "@" waspStylePath
  let (extensionLessFile, extType) = splitExtensionType relFile
  return $ ExtFileCachePath (SP.fromPathRelFile extensionLessFile) extType

absPathToCachePath :: LSP.MonadLsp c m => SP.Path' SP.Abs (SP.File a) -> m (Maybe ExtFileCachePath)
absPathToCachePath absFile = do
  -- Makes the path relative to src/ and deletes the extension.
  maybeProjectDir <- (>>= SP.parseAbsDir) <$> LSP.getRootPath
  case maybeProjectDir of
    Nothing -> pure Nothing
    Just (projectRootDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)) ->
      let srcDir = projectRootDir SP.</> srcDirInProjectRootDir
       in case P.stripProperPrefix (SP.toPathAbsDir srcDir) (SP.toPathAbsFile absFile) of
            Nothing -> pure Nothing
            Just relFile -> do
              let (extensionLessFile, extType) = splitExtensionType relFile
              pure $ Just $ ExtFileCachePath (SP.fromPathRelFile extensionLessFile) extType

cachePathToAbsPathWithoutExt :: LSP.MonadLsp c m => ExtFileCachePath -> m (Maybe (SP.Path' SP.Abs (SP.File ExtensionlessExtFile)))
cachePathToAbsPathWithoutExt (ExtFileCachePath cachePath _) = do
  -- Converts to an absolute path, but does not add any extension.
  maybeProjectDir <- (>>= SP.parseAbsDir) <$> LSP.getRootPath
  case maybeProjectDir of
    Nothing -> return Nothing
    Just (projectRootDir :: SP.Path' SP.Abs (SP.Dir WaspProjectDir)) -> do
      return $ Just $ projectRootDir SP.</> srcDirInProjectRootDir SP.</> cachePath

cachePathToAbsPath :: forall m c a. LSP.MonadLsp c m => ExtFileCachePath -> m (Maybe (SP.Path' SP.Abs (SP.File a)))
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

-- | Try to find the @tsconfig.json@ file based on the location of the given
-- file.
--
-- Returns either @src/client/tsconfig.json@ or @src/server/tsconfig.json@,
-- depending on which directory the file is in. Does not check if those
-- config files exist.
--
-- IF the given path is not in either @src/@ subdirectory, returns nothing.
tryGetTsconfigForAbsPath :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> SP.Path' SP.Abs (SP.File a) -> Maybe (SP.Path' SP.Abs (SP.File a))
tryGetTsconfigForAbsPath projectRootDir file = tsconfigPath [SP.reldir|src/client|] <|> tsconfigPath [SP.reldir|src/server|]
  where
    tsconfigPath :: SP.Path' (SP.Rel WaspProjectDir) SP.Dir' -> Maybe (SP.Path' SP.Abs (SP.File a))
    tsconfigPath folder =
      let absFolder = projectRootDir SP.</> folder
       in if SP.toPathAbsDir absFolder `P.isProperPrefixOf` SP.toPathAbsFile file
            then Just $ absFolder SP.</> [SP.relfile|tsconfig.json|]
            else Nothing

srcDirInProjectRootDir :: SP.Path' (SP.Rel WaspProjectDir) (SP.Dir SourceExternalCodeDir)
srcDirInProjectRootDir = [SP.reldir|src|]

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

-- | Convert an extension (like @\".js\"@) to an 'ExtensionType'.
extensionType :: String -> ExtensionType
extensionType ".js" = DotTJS
extensionType ".jsx" = DotTJSX
extensionType ext = DotExact ext

-- | Split a path into a path with no extension and an extension type. If the
-- file has no extension, it gives it 'DotAnyTS' extension type.
splitExtensionType :: P.Path b P.File -> (P.Path b P.File, ExtensionType)
splitExtensionType file = case P.splitExtension file of
  Nothing -> (file, DotAnyTS)
  Just (file', ext) -> (file', extensionType ext)

-- | The extensions that each 'ExtensionType' represents.
allowedExts :: ExtensionType -> [String]
allowedExts DotTJS = [".ts", ".js"]
allowedExts DotTJSX = [".tsx", ".jsx"]
allowedExts DotAnyTS = [".ts", ".js", ".tsx", ".jsx"]
allowedExts (DotExact ext) = [ext]

replaceExtension :: SP.Path' SP.Abs (SP.File a) -> String -> SP.Path' SP.Abs (SP.File a)
replaceExtension path ext =
  SP.fromPathAbsFile $ fromMaybe (error "invalid extension") $ P.replaceExtension ext $ SP.toPathAbsFile path

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
