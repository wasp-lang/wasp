module Wasp.Util.Network.URI
  ( showUri,
  )
where

import Network.URI (URI, uriToString)

showUri :: URI -> String
showUri uri = uriToString id uri ""
