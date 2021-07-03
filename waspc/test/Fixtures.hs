module Fixtures where

import Data.Maybe (fromJust)
import qualified Path as P
import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp
import qualified Wasp.Route as RouteAST

app :: App
app =
  App
    { appName = "test_app",
      appTitle = "Hello World!",
      appHead = Nothing
    }

routeHome :: RouteAST.Route
routeHome =
  RouteAST.Route
    { RouteAST._urlPath = "/home",
      RouteAST._targetPage = "Home"
    }

wasp :: Wasp
wasp =
  fromWaspElems
    [ WaspElementApp app
    ]

systemSPRoot :: SP.Path' SP.Abs (SP.Dir d)
systemSPRoot = fromJust $ SP.parseAbsDir systemFpRoot

systemPathRoot :: P.Path P.Abs P.Dir
systemPathRoot = fromJust $ P.parseAbsDir systemFpRoot

systemFpRoot :: FilePath
systemFpRoot = if FP.pathSeparator == '\\' then "C:\\" else "/"
