module Wasp.Cli.Util.UrlArgument
  ( appComponentUrlOption,
    parseAppComponentUrl,
  )
where

import Data.Maybe (isJust)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import qualified Options.Applicative as Opt

-- | Option for the URL at which an app component ("client" or "server") is
-- reachable, e.g. @--client-url@.
appComponentUrlOption :: String -> Opt.Parser (Maybe String)
appComponentUrlOption componentName =
  Opt.optional $
    Opt.option
      (Opt.str >>= either Opt.readerError return . parseAppComponentUrl)
      ( Opt.long (componentName ++ "-url")
          <> Opt.metavar "URL"
          <> Opt.help helpText
      )
  where
    helpText =
      "URL at which the " ++ componentName ++ " is reachable, if not http://localhost:<" ++ componentName ++ "-port>"

-- | Checks that a string is a URL at which an app component can be reached:
-- an absolute http(s) URL with a host, and without a query or a fragment.
-- Returns the URL unchanged on success.
parseAppComponentUrl :: String -> Either String String
parseAppComponentUrl input = case parseAbsoluteURI input of
  Nothing -> Left $ show input ++ " is not a valid absolute URL"
  Just uri
    | uriScheme uri `notElem` ["http:", "https:"] -> Left $ show input ++ " must start with http:// or https://"
    | not (hasHost uri) -> Left $ show input ++ " must contain a host"
    | not (null $ uriQuery uri) || not (null $ uriFragment uri) -> Left $ show input ++ " must not contain a query or a fragment"
    | otherwise -> Right input
  where
    hasHost uri = isJust $ uriAuthority uri >>= nonEmpty . uriRegName
    nonEmpty "" = Nothing
    nonEmpty s = Just s
