module Wasp.Cli.Util.UrlArgument
  ( urlOption,
  )
where

import Data.Char (toLower)
import Network.URI (URI, parseAbsoluteURI, uriAuthority, uriQuery, uriRegName, uriScheme, uriUserInfo)
import qualified Options.Applicative as Opt

-- | Defines the parser for a flag that takes the public URL of one of the app's
-- components, e.g. `--server-url https://myapp.loca.lt`.
urlOption :: String -> String -> Opt.Parser (Maybe URI)
urlOption optionName helpText =
  Opt.optional $
    Opt.option
      (Opt.eitherReader httpUrlFromString)
      ( Opt.long optionName
          <> Opt.metavar "URL"
          <> Opt.help helpText
      )

-- | Converts a string to a URI, rejecting anything we can't use as the public
-- URL of an app component.
httpUrlFromString :: String -> Either String URI
httpUrlFromString input = case parseAbsoluteURI input of
  -- The checks below have to happen in this order: `localhost:3001` parses as a
  -- URL with the `localhost:` scheme, and `http://` parses as one with an empty
  -- host.
  Nothing -> failure "it is not an absolute URL"
  Just uri
    | map toLower (uriScheme uri) `notElem` ["http:", "https:"] ->
        failure "it must start with http:// or https://"
    | maybe True (null . uriRegName) (uriAuthority uri) ->
        failure "it must include a host"
    | not (null $ uriQuery uri) ->
        failure "it must not have a query string"
    -- We render URIs with `show`, which masks the password in the user info,
    -- so a URL with one wouldn't survive the round trip.
    | maybe False (not . null . uriUserInfo) (uriAuthority uri) ->
        failure "it must not have a username or a password"
    | otherwise -> Right uri
  where
    failure reason =
      Left $
        "Invalid URL '"
          ++ input
          ++ "': "
          ++ reason
          ++ "."
