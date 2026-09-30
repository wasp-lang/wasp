module Wasp.AppComponentUrl
  ( AppComponentUrl (..),
    makeAppComponentUrl,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI, uriToString)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

-- | Where an app component (client or server) listens, and the URLs under which
-- it is reachable.
data AppComponentUrl = AppComponentUrl
  { port :: PortNumber,
    -- | The URL under which the app component is reachable: the custom URL the
    -- user chose, or the localhost one.
    url :: String,
    -- | The localhost URL of the app component, even when it has a custom URL.
    localUrl :: String
  }
  deriving (Show, Eq)

makeAppComponentUrl :: PortNumber -> Maybe (Path Posix Abs (Dir ())) -> Maybe URI -> AppComponentUrl
makeAppComponentUrl port path customUrl = AppComponentUrl {port, url, localUrl}
  where
    url = maybe localUrl (\uri -> uriToString id uri "") customUrl
    localUrl = concat $ ["http://localhost:", show port] ++ [SP.fromAbsDirP p | Just p <- [path]]
