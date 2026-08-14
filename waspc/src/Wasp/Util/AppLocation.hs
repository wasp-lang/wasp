module Wasp.Util.AppLocation where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

data AppLocation
  = Local
  { port :: PortNumber,
    baseDir :: Maybe (Path Posix Abs (Dir ()))
  }

host :: AppLocation -> String
host (Local {}) = "localhost"

protocol :: AppLocation -> String
protocol (Local {}) = "http"

url :: AppLocation -> String
url loc =
  concat $
    [protocol loc, "://", host loc, ":", show $ port loc]
      ++ [SP.fromAbsDirP bd | Just bd <- [loc.baseDir]]
