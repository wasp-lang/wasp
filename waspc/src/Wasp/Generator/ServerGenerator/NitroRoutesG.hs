-- | Generates the code that lets Nitro (the server serving the app) serve
-- Wasp's Express app: an h3 handler wrapping the Express app, the list of URL
-- prefixes Express is given a chance to answer, and the Nitro plugin running
-- everything of the app's server that isn't answering a request.
module Wasp.Generator.ServerGenerator.NitroRoutesG
  ( genNitro,
    serverEntryFileInServerRootDir,
    waspPluginFileInServerRootDir,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.ApiNamespace as ApiNamespace
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import qualified Wasp.Generator.Crud.Routes as CrudRoutes
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.WebSocket as WS
import Wasp.Util ((<++>))

genNitro :: AppSpec -> Generator [FileDraft]
genNitro spec =
  sequence
    [ return $ C.mkTmplFd [relfile|src/nitro/expressBridge.ts|],
      genApiManifest spec,
      genServerEntry spec,
      genWaspPlugin spec
    ]
    <++> genSetup spec

genApiManifest :: AppSpec -> Generator FileDraft
genApiManifest spec = return $ C.mkTmplFdWithData [relfile|src/nitro/apiManifest.ts|] (Just tmplData)
  where
    tmplData = object ["bridgedPathPrefixes" .= makeJsArrayFromHaskellList (getBridgedPathPrefixes spec)]

genServerEntry :: AppSpec -> Generator FileDraft
genServerEntry spec = return $ C.mkTmplFdWithData (C.asTmplFile serverEntryFileInServerRootDir) (Just tmplData)
  where
    tmplData =
      object
        [ "setupFn" .= extImportToImportJson pathFromNitroDirToServerSrcDir (getSetupFn spec),
          "areWebSocketsUsed" .= WS.areWebSocketsUsed spec
        ]

-- | Runs the user's server setup function against the Express app Nitro serves.
-- Only generated for apps that have one, and imported by the plugin below when
-- the server starts.
genSetup :: AppSpec -> Generator [FileDraft]
genSetup spec = case getSetupFn spec of
  Nothing -> return []
  Just setupFn -> return [C.mkTmplFdWithData [relfile|src/nitro/setup.ts|] (Just tmplData)]
    where
      tmplData = object ["setupFn" .= extImportToImportJson pathFromNitroDirToServerSrcDir (Just setupFn)]

-- | The Nitro plugin running everything of the app's server that isn't
-- answering a request: its job queue, the user's server setup function, and
-- stopping all of it when the server stops.
genWaspPlugin :: AppSpec -> Generator FileDraft
genWaspPlugin spec = return $ C.mkTmplFdWithData (C.asTmplFile waspPluginFileInServerRootDir) (Just tmplData)
  where
    tmplData =
      object
        [ "setupFn" .= extImportToImportJson pathFromNitroPluginsDirToServerSrcDir (getSetupFn spec),
          "isPgBossJobExecutorUsed" .= isPgBossJobExecutorUsed spec
        ]

getSetupFn :: AppSpec -> Maybe EI.ExtImport
getSetupFn spec = AS.App.Server.setupFn =<< AS.App.server (snd $ getApp spec)

-- | The URL prefixes we let Express answer. Everything else is a request for one
-- of the app's pages, and never enters Express.
--
-- An app whose server setup function can add routes of its own gets a single
-- catch-all prefix: we have no way of knowing what it added, and this list is
-- only there to keep page requests out of Express, never to decide what Express
-- answers (Express falls through to the app's pages on its own).
getBridgedPathPrefixes :: AppSpec -> [String]
getBridgedPathPrefixes spec
  | isJust (getSetupFn spec) = ["/"]
  | otherwise =
      nub $
        concat
          [ [C.healthRoutePath],
            ["/" ++ C.operationsRouteInRootRouter],
            ["/auth" | isAuthEnabled spec],
            ["/" ++ CrudRoutes.crudRouteInRootRouter | not . null $ AS.getCruds spec],
            map (getStaticPathPrefix . Api.path . snd) (AS.getApis spec),
            map (getStaticPathPrefix . ApiNamespace.path . snd) (AS.getApiNamespaces spec)
          ]

-- | The part of an Express route path that is a plain URL prefix, e.g.
-- @\/foo\/:id@ becomes @\/foo@.
--
-- Custom API paths are Express route patterns, and only Express knows how to
-- match those, so we cut them short at the first segment that isn't plain text.
getStaticPathPrefix :: String -> String
getStaticPathPrefix path = case staticPrefix of
  "" -> "/"
  prefix -> prefix
  where
    staticPrefix = intercalate "/" . takeWhile isStaticSegment . splitOn "/" $ path
    isStaticSegment = not . any (`elem` expressPathPatternChars)
    -- Everything Express 5 (path-to-regexp) gives a meaning to inside a route
    -- path, plus a couple of characters older versions used, just in case.
    expressPathPatternChars = ":*(){}[]?+" :: String

-- | Nitro's server entry point, which the app's Vite config points Nitro at.
serverEntryFileInServerRootDir :: Path' (Rel C.ServerRootDir) File'
serverEntryFileInServerRootDir = [relfile|src/nitro/serverEntry.ts|]

-- | Wasp's Nitro plugin, which the app's Vite config points Nitro at.
waspPluginFileInServerRootDir :: Path' (Rel C.ServerRootDir) File'
waspPluginFileInServerRootDir = [relfile|src/nitro/plugins/wasp.ts|]

pathFromNitroDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
pathFromNitroDirToServerSrcDir = [reldirP|../|]

pathFromNitroPluginsDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
pathFromNitroPluginsDirToServerSrcDir = [reldirP|../../|]
