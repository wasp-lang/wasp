module Wasp.Project.RunConfig
  ( ProjectRunConfig (..),
    makeLocalUrl,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI (..), URIAuth (..))
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

-- | URLs used to reach each component and ports on which they listen.
-- These can differ when a component runs behind a proxy.
data ProjectRunConfig = ProjectRunConfig
  { clientUrl :: URI,
    serverUrl :: URI,
    clientPort :: PortNumber,
    serverPort :: PortNumber
  }
  deriving (Show, Eq)

-- | The URL under which a component listening on the given port is reachable
-- on this machine.
makeLocalUrl :: PortNumber -> Maybe (Path Posix Abs (Dir ())) -> URI
makeLocalUrl port path =
  URI
    { uriScheme = "http:",
      uriAuthority = Just $ URIAuth "" "localhost" (":" ++ show port),
      uriPath = maybe "" SP.fromAbsDirP path,
      uriQuery = "",
      uriFragment = ""
    }
