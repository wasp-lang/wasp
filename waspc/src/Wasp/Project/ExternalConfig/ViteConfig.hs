{-# LANGUAGE QuasiQuotes #-}

module Wasp.Project.ExternalConfig.ViteConfig
  ( validateViteConfig,
    findViteConfigFile,
  )
where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, File', Path', Rel, relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Validation (Validation (..))
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil

validateViteConfig :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] ())
validateViteConfig waspDir =
  findViteConfigFile waspDir >>= \case
    Nothing -> return $ Failure [fileNotFoundMessage]
    Just viteConfigFile -> validatePluginImports $ waspDir </> viteConfigFile
  where
    validatePluginImports :: Path' Abs File' -> IO (Validation [CompileError] ())
    validatePluginImports viteConfigFile = do
      content <- IOUtil.readFileStrict viteConfigFile
      return $ case filter (not . (`T.isInfixOf` content) . fst) requiredPluginImports of
        [] -> Success ()
        missingImports -> Failure $ map snd missingImports

    requiredPluginImports :: [(T.Text, CompileError)]
    requiredPluginImports =
      [ (clientPluginImportModule, missingClientPluginImportMessage),
        (serverPluginImportModule, missingServerPluginImportMessage)
      ]

    fileNotFoundMessage :: CompileError
    fileNotFoundMessage =
      T.unpack
        [trimming|
          Couldn't find `vite.config.ts` (or `vite.config.js`) in the project directory.
          Wasp requires a Vite config file with the `wasp` and `waspServer` plugins configured.
          Read more: ${viteConfigDocsUrl}
        |]

    missingClientPluginImportMessage :: CompileError
    missingClientPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp Vite plugin from "${clientPluginImportModule}".
          The `wasp` plugin is required for Wasp to work correctly.
          Read more: ${viteConfigDocsUrl}
        |]

    missingServerPluginImportMessage :: CompileError
    missingServerPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp server Vite plugin from "${serverPluginImportModule}".
          The `waspServer` plugin is required for Wasp to build your server. Add it to your Vite config:

            import { wasp } from "${clientPluginImportModule}";
            import { waspServer } from "${serverPluginImportModule}";

            export default defineConfig({
              plugins: [wasp(), waspServer()],
            });

          Read more: ${viteConfigDocsUrl}
        |]

    clientPluginImportModule :: T.Text
    clientPluginImportModule = "wasp/client/vite"

    serverPluginImportModule :: T.Text
    serverPluginImportModule = "wasp/server/vite"

    viteConfigDocsUrl :: T.Text
    viteConfigDocsUrl = "https://wasp.sh/docs/project/custom-vite-config#required-configuration"

-- | Finds the user's Vite config file, if there is one.
findViteConfigFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' (Rel WaspProjectDir) File'))
findViteConfigFile waspDir = findExistingFile viteConfigCandidates
  where
    viteConfigCandidates :: [Path' (Rel WaspProjectDir) File']
    viteConfigCandidates =
      [ [relfile|vite.config.ts|],
        [relfile|vite.config.js|]
      ]

    findExistingFile :: [Path' (Rel WaspProjectDir) File'] -> IO (Maybe (Path' (Rel WaspProjectDir) File'))
    findExistingFile [] = return Nothing
    findExistingFile (f : fs) = do
      exists <- doesFileExist $ toFilePath (waspDir </> f)
      if exists then return (Just f) else findExistingFile fs
