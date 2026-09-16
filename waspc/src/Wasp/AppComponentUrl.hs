module Wasp.AppComponentUrl
  ( AppComponentUrl (..),
    host,
    protocol,
    url,
  )
where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

data AppComponentUrl = Local
  { port :: PortNumber,
    path :: Maybe (Path Posix Abs (Dir ()))
  }
  deriving (Show, Eq)

host :: AppComponentUrl -> String
host (Local {}) = "localhost"

protocol :: AppComponentUrl -> String
protocol (Local {}) = "http"

url :: AppComponentUrl -> String
url loc =
  concat $
    [protocol loc, "://", host loc, ":", show $ port loc]
      ++ [SP.fromAbsDirP p | Just p <- [loc.path]]
