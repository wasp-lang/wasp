module Wasp.AppComponentUrl
  ( AppComponentUrl (..),
    url,
    localUrl,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI, uriToString)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

-- | Where an app component (client or server) listens, and the URL under which
-- it is reachable.
data AppComponentUrl = AppComponentUrl
  { port :: PortNumber,
    path :: Maybe (Path Posix Abs (Dir ())),
    -- | The URL the user chose to reach the app component at, instead of
    -- localhost.
    customUrl :: Maybe URI
  }
  deriving (Show, Eq)

-- | The URL under which the app component is reachable.
url :: AppComponentUrl -> String
url appComponentUrl =
  maybe (localUrl appComponentUrl) (\uri -> uriToString id uri "") appComponentUrl.customUrl

-- | The localhost URL of the app component, even when it has a custom URL.
localUrl :: AppComponentUrl -> String
localUrl appComponentUrl =
  concat $
    ["http://localhost:", show appComponentUrl.port]
      ++ [SP.fromAbsDirP p | Just p <- [appComponentUrl.path]]
