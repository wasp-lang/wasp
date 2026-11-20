module Wasp.Generator.WebAppGenerator.NodeCommands
  ( runWebAppDevServer,
    buildWebApp,
    previewWebApp,
  )
where

import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.Project.Common (WaspProjectDir)

runWebAppDevServer :: NodeCommand WaspProjectDir
runWebAppDevServer = NodeCommand "vite" []

buildWebApp :: NodeCommand WaspProjectDir
buildWebApp = NodeCommand "vite" ["build"]

previewWebApp :: NodeCommand WaspProjectDir
previewWebApp =
  NodeCommand
    "vite"
    [ "preview", -- `preview` launches a static file server for the built client.
      "--strictPort" -- This will make it fail if the port is already in use.
    ]
