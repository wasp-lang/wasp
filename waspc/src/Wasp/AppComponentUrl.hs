module Wasp.AppComponentUrl
  ( AppComponentUrl (..),
    url,
    isCustom,
  )
where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP

-- | Where an app component (client or server) listens, and the URL under which
-- it is reachable.
data AppComponentUrl
  = -- | Reachable on localhost, at the port it listens on.
    Local
      { port :: PortNumber,
        path :: Maybe (Path Posix Abs (Dir ()))
      }
  | -- | Listens on a local port, but is reachable at a URL the user chose (e.g.
    -- a LAN hostname or an HTTPS tunnel that forwards to the local port).
    Custom
      { port :: PortNumber,
        publicUrl :: String
      }
  deriving (Show, Eq)

url :: AppComponentUrl -> String
url Local {port = port', path = path'} =
  concat $
    ["http://localhost:", show port']
      ++ [SP.fromAbsDirP p | Just p <- [path']]
url Custom {publicUrl = publicUrl'} = publicUrl'

isCustom :: AppComponentUrl -> Bool
isCustom Custom {} = True
isCustom Local {} = False
