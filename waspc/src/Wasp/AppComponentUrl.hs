module Wasp.AppComponentUrl
  ( AppComponentUrl (..),
    host,
    protocol,
    origin,
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

-- | The URL without its path, e.g. "http://localhost:3000".
origin :: AppComponentUrl -> String
origin loc = concat [protocol loc, "://", host loc, ":", show $ port loc]

url :: AppComponentUrl -> String
url loc = origin loc ++ concat [SP.fromAbsDirP p | Just p <- [loc.path]]
