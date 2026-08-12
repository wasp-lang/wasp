{-# LANGUAGE QuasiQuotes #-}

module Wasp.Project.ExternalConfig.ViteConfig
  ( validateViteConfig,
    findViteConfigFileInWaspProjectDir,
  )
where

import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
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

-- | Finds the project's Vite config file, which is written either in
-- TypeScript or in JavaScript.
findViteConfigFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Maybe (Path' (Rel WaspProjectDir) File'))
findViteConfigFileInWaspProjectDir waspDir =
  listToMaybe <$> filterM (doesFileExist . toFilePath . (waspDir </>)) viteConfigFileCandidates
  where
    viteConfigFileCandidates :: [Path' (Rel WaspProjectDir) File']
    viteConfigFileCandidates =
      [ [relfile|vite.config.ts|],
        [relfile|vite.config.js|]
      ]

validateViteConfig :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] ())
validateViteConfig waspDir =
  findViteConfigFileInWaspProjectDir waspDir >>= \case
    Nothing -> return $ Failure [fileNotFoundMessage]
    Just path -> validatePluginImport $ waspDir </> path
  where
    validatePluginImport :: Path' Abs File' -> IO (Validation [CompileError] ())
    validatePluginImport viteConfigFile = do
      content <- IOUtil.readFileStrict viteConfigFile
      return $
        if waspPluginImportModule `T.isInfixOf` content
          then Success ()
          else Failure [missingPluginImportMessage]

    fileNotFoundMessage :: CompileError
    fileNotFoundMessage =
      T.unpack
        [trimming|
          Couldn't find `vite.config.ts` (or `vite.config.js`) in the project directory.
          Wasp requires a Vite config file with the `wasp` plugin configured.
          Read more: ${viteConfigDocsUrl}
        |]

    missingPluginImportMessage :: CompileError
    missingPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp Vite plugin from "${waspPluginImportModule}".
          The `wasp` plugin is required for Wasp to work correctly.
          Read more: ${viteConfigDocsUrl}
        |]

    waspPluginImportModule :: T.Text
    waspPluginImportModule = "wasp/client/vite"

    viteConfigDocsUrl :: T.Text
    viteConfigDocsUrl = "https://wasp.sh/docs/project/custom-vite-config#required-configuration"
