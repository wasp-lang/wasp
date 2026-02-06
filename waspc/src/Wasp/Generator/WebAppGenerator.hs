{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInProjectRootDir,
    viteBuildDirInWebAppDir,
    viteSsrBuildDirInWebAppDir,
    createWebAppRootDir,
    genWebApp,
  )
where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import StrongPath (Abs, Dir, File, Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Page as AS.Page
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft, createTextFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Generator.Monad (Generator)

data WebAppRootDir

data WebAppViteBuildDir
data WebAppViteSsrBuildDir

-- We require the `web-app` dir to be generated for deployment purposes.
-- Our deployment tooling expects to have a folder outside of the user project
-- dir where e.g. Dockerfile for static server or Staticfile can be created.
webAppRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = [reldir|web-app|]

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]

viteSsrBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteSsrBuildDir)
viteSsrBuildDirInWebAppDir = [reldir|build-ssr|]

createWebAppRootDir :: Path' Abs (Dir ProjectRootDir) -> IO ()
createWebAppRootDir projectDir = createDirectoryIfMissing True webAppRootDir
  where
    webAppRootDir = SP.fromAbsDir $ projectDir </> webAppRootDirInProjectRootDir

genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec =
  sequence
    [ genServerSsr,
      genSsrPackageJson,
      genSsrConfig spec
    ]

genServerSsr :: Generator FileDraft
genServerSsr =
  return $
    createTemplateFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|server-ssr.mjs|])
      ([relfile|web-app/server-ssr.mjs|] :: Path' (Rel TemplatesDir) (File ()))
      Nothing

genSsrPackageJson :: Generator FileDraft
genSsrPackageJson =
  return $
    createTemplateFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|package.json|])
      ([relfile|web-app/ssr-package.json|] :: Path' (Rel TemplatesDir) (File ()))
      Nothing

genSsrConfig :: AppSpec -> Generator FileDraft
genSsrConfig spec =
  return $
    createTextFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|ssr.json|])
      (TE.decodeUtf8 $ BL.toStrict $ encode ssrConfig)
  where
    ssrConfig =
      object
        [ "enabled" .= hasSsrEnabledPage
        ]
    hasSsrEnabledPage =
      any (maybe False id . AS.Page.ssr . snd) (AS.getPages spec)
