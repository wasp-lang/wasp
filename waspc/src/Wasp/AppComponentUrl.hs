module Wasp.AppComponentUrl where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

data AppComponentUrl = Local
  { port :: PortNumber,
    baseDir :: Maybe (Path Posix Abs (Dir ()))
  }

host :: AppComponentUrl -> String
host (Local {}) = "localhost"

protocol :: AppComponentUrl -> String
protocol (Local {}) = "http"

url :: AppComponentUrl -> String
url loc =
  concat $
    [protocol loc, "://", host loc, ":", show $ port loc]
      ++ [SP.fromAbsDirP bd | Just bd <- [loc.baseDir]]
