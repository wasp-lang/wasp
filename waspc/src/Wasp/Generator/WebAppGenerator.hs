module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInProjectRootDir,
    viteBuildDirInWebAppDir,
    viteSsrBuildDirInWebAppDir,
    createWebAppRootDir,
    genWebApp,
    hasSsrEnabledPage,
    npmDepsFromWasp,
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
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp

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
  sequence $
    [ genSsrConfig ssrEnabled
    ]
      ++ if ssrEnabled
        then
          [ genServerSsr spec,
            genSsrPackageJson
          ]
        else []
  where
    ssrEnabled = hasSsrEnabledPage spec

genServerSsr :: AppSpec -> Generator FileDraft
genServerSsr spec =
  return $
    createTemplateFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|server-ssr.mjs|])
      ([relfile|web-app/server-ssr.mjs|] :: Path' (Rel TemplatesDir) (File ()))
      (Just tmplData)
  where
    baseDir = SP.fromAbsDirP (WebApp.getBaseDir spec)
    tmplData =
      object
        [ -- Must be JSON-encoded because the template injects it unescaped to avoid HTML escaping.
          "baseDirJson" .= (TE.decodeUtf8 $ BL.toStrict $ encode baseDir)
        ]

genSsrPackageJson :: Generator FileDraft
genSsrPackageJson =
  return $
    createTemplateFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|package.json|])
      ([relfile|web-app/ssr-package.json|] :: Path' (Rel TemplatesDir) (File ()))
      Nothing

hasSsrEnabledPage :: AppSpec -> Bool
hasSsrEnabledPage spec =
  any (maybe False id . AS.Page.ssr . snd) (AS.getPages spec)

-- Keep in sync with `data/Generator/templates/web-app/ssr-package.json`.
ssrServerSirvVersion :: String
ssrServerSirvVersion = "3.0.2"

npmDepsFromWasp :: AppSpec -> N.NpmDepsForPackage
npmDepsFromWasp spec =
  N.NpmDepsForPackage
    { N.dependencies =
        if hasSsrEnabledPage spec
          then Npm.Dependency.fromList [("sirv", ssrServerSirvVersion)]
          else [],
      N.devDependencies = [],
      N.peerDependencies = []
    }

genSsrConfig :: Bool -> Generator FileDraft
genSsrConfig ssrEnabled =
  return $
    createTextFileDraft
      (webAppRootDirInProjectRootDir </> [relfile|ssr.json|])
      (TE.decodeUtf8 $ BL.toStrict $ encode ssrConfig)
  where
    ssrConfig =
      object
        [ "enabled" .= ssrEnabled
        ]
