module Wasp.Cli.Util.AppUrls
  ( showAppUrls,
  )
where

import Wasp.Util.AppLocation (AppLocation)
import qualified Wasp.Util.AppLocation as AL

showAppUrls :: AppLocation -> AppLocation -> String
showAppUrls clientLocation serverLocation =
  unlines
    [ " ℹ Client: " ++ ensureTrailingSlash (AL.url clientLocation),
      " ℹ Server: " ++ ensureTrailingSlash (AL.url serverLocation)
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
