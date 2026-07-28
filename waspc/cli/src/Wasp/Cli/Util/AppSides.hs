{-# LANGUAGE DeriveTraversable #-}

module Wasp.Cli.Util.AppSides where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp

data AppSides a = AppSides
  { client :: a,
    server :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative AppSides where
  pure x = AppSides x x
  AppSides f1 f2 <*> AppSides x1 x2 = AppSides (f1 x1) (f2 x2)

names :: AppSides String
names =
  AppSides
    { client = "client",
      server = "server"
    }

defaultDevUrls :: AppSpec -> AppSides String
defaultDevUrls spec =
  AppSides
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

defaultPorts :: AppSides Int
defaultPorts =
  AppSides
    { client = WebApp.defaultClientPort,
      server = Server.defaultServerPort
    }
