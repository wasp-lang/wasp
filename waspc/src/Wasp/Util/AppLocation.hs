module Wasp.Util.AppLocation where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

data AppLocation
  = Local
      { port :: PortNumber,
        baseDir :: Maybe (Path Posix Abs (Dir ()))
      }
  | Remote
      { secure' :: Bool,
        host' :: String,
        port :: PortNumber,
        baseDir :: Maybe (Path Posix Abs (Dir ()))
      }

host :: AppLocation -> String
host (Local {}) = "localhost"
host (Remote {host' = h}) = h

protocol :: AppLocation -> String
protocol (Remote {secure' = s}) | s = "https"
protocol _ = "http"

url :: AppLocation -> String
url loc =
  concat $
    [ protocol loc,
      "://",
      host loc,
      ":",
      show (port loc)
    ]
      ++ maybe
        []
        (\bd -> ["/", SP.fromAbsDirP bd])
        loc.baseDir
