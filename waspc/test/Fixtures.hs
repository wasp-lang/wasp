module Fixtures where

import qualified Path as P
import Data.Maybe (fromJust)
import qualified System.FilePath as FP

import Wasp
import qualified Wasp.Route as RouteAST

app :: App
app = App
    { appName = "test_app"
    , appTitle = "Hello World!"
    }

routeHome :: RouteAST.Route
routeHome = RouteAST.Route
    { RouteAST._urlPath = "/home"
    , RouteAST._targetPage = "Home"
    }

wasp :: Wasp
wasp = fromWaspElems
    [ WaspElementApp app
    ]

fpRoot :: P.Path P.Abs P.Dir
fpRoot = fromJust $ P.parseAbsDir $ if FP.pathSeparator == '\\' then "C:\\" else "/"
