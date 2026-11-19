module Wasp.Generator.ImportAliases
  ( writeImportAliasesTsConfig,
    readSdkExports, -- Exported for testing.
    makeSdkImportAliases, -- Exported for testing.
  )
where

import Control.Monad (unless)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath.Posix as FP.Posix
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.SdkGenerator.Common
  ( sdkPackageJsonInSdkTemplatesDir,
    sdkPackageJsonTmplData,
    sdkPackageName,
    sdkRootDirInGeneratedAppDir,
    sdkTemplatesDirInTemplatesDir,
  )
import Wasp.Generator.Templates (compileAndRenderTemplate)
import qualified Wasp.Util.IO as IOUtil

-- | An import specifier, e.g. @wasp/client/operations@, and the paths
-- TypeScript resolves it to.
type ImportAliases = M.Map String [FilePath]

-- | Writes the tsconfig that maps the `wasp/*` imports to the generated SDK,
-- for the generated server's tsconfig to extend. The SDK isn't a package in
-- `node_modules`, so TypeScript can't find it on its own.
--
-- The project's own tsconfig files extend the tsconfig that comes with
-- \@wasp/wasp instead, which exists before Wasp generates anything. A test
-- keeps the two in sync. Bundlers resolve the same imports with
-- @resolveSdkImport@ of \@wasp/sdk-core.
writeImportAliasesTsConfig :: Path' Abs (Dir GeneratedAppDir) -> IO ()
writeImportAliasesTsConfig generatedAppDir = do
  importAliases <- makeSdkImportAliases <$> readSdkExports
  let newContent =
        TL.toStrict . TLE.decodeUtf8 . AesonPretty.encodePretty $
          object ["compilerOptions" .= object ["paths" .= importAliases]]
  -- Editors and `tsc` reload a tsconfig whenever it's written.
  isUpToDate <- (== Just newContent) <$> tryReadingFile
  unless isUpToDate $ do
    createDirectoryIfMissing True $ SP.fromAbsDir generatedAppDir
    IOUtil.writeFileFromText importAliasesTsConfigFile newContent
  where
    importAliasesTsConfigFile = generatedAppDir </> importAliasesTsConfigInGeneratedAppDir

    tryReadingFile = do
      doesFileExist <- IOUtil.doesFileExist importAliasesTsConfigFile
      if doesFileExist
        then Just <$> IOUtil.readFileStrict importAliasesTsConfigFile
        else return Nothing

importAliasesTsConfigInGeneratedAppDir :: Path' (Rel GeneratedAppDir) File'
importAliasesTsConfigInGeneratedAppDir = [relfile|tsconfig.paths.json|]

-- | The `exports` of the SDK's package.json: a subpath like
-- @./client/operations@, and the file it points at.
type SdkExports = M.Map String FilePath

-- | The SDK's package.json template is the only place that lists its exports.
readSdkExports :: IO SdkExports
readSdkExports = do
  packageJsonText <-
    compileAndRenderTemplate
      (sdkTemplatesDirInTemplatesDir </> sdkPackageJsonInSdkTemplatesDir)
      sdkPackageJsonTmplData
  return
    $ either (error . ("The SDK's package.json template has no valid `exports`: " ++)) sdkPackageJsonExports
    $ Aeson.eitherDecodeStrict
    $ TE.encodeUtf8 packageJsonText

newtype SdkPackageJson = SdkPackageJson {sdkPackageJsonExports :: SdkExports}

instance Aeson.FromJSON SdkPackageJson where
  parseJSON = Aeson.withObject "SdkPackageJson" $ \packageJson ->
    SdkPackageJson <$> packageJson Aeson..: "exports"

-- | TypeScript resolves the paths relative to the tsconfig that lists them,
-- which is in the generated app dir.
makeSdkImportAliases :: SdkExports -> ImportAliases
makeSdkImportAliases = M.fromList . map makeImportAlias . M.toList
  where
    makeImportAlias (exportSubpath, exportedFile) =
      ( sdkPackageName FP.Posix.</> dropCurrentDirPrefix exportSubpath,
        ["./" ++ sdkRootDirFromGeneratedAppDir FP.Posix.</> dropJsExtension (dropCurrentDirPrefix exportedFile)]
      )

    sdkRootDirFromGeneratedAppDir = SP.fromRelDirP $ fromMaybe (error "The SDK dir is a POSIX path") $ SP.relDirToPosix sdkRootDirInGeneratedAppDir

    dropCurrentDirPrefix path = fromMaybe path $ stripPrefix "./" path

    -- Without the extension TypeScript looks for the declaration file next to
    -- the JS file, also for a `.jsx` one.
    dropJsExtension path
      | FP.Posix.takeExtension path `elem` [".js", ".jsx"] = FP.Posix.dropExtension path
      | otherwise = path
