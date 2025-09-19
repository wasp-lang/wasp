module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    webAppPackageName,
  )
where

import Wasp.AppSpec (AppSpec, isBuild)

serverPackageName :: AppSpec -> String
serverPackageName = workspacePackageName "server"

webAppPackageName :: AppSpec -> String
webAppPackageName = workspacePackageName "webapp"

workspacePackageName :: String -> AppSpec -> String
workspacePackageName baseName spec = "@wasp.sh/generated-" ++ baseName ++ "-" ++ mode
  where
    mode = if isBuild spec then "build" else "dev"
