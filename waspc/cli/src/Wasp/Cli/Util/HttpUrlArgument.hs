module Wasp.Cli.Util.HttpUrlArgument
  ( httpUrlParser,
    parseHttpUrl,
  )
where

import Data.Either (isRight)
import Data.Maybe (isJust)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (parsePort)

httpUrlParser :: String -> String -> Opt.Parser URI
httpUrlParser optionName helpText =
  Opt.option
    (Opt.eitherReader parseHttpUrl)
    ( Opt.long optionName
        <> Opt.metavar "URL"
        <> Opt.help helpText
    )

-- | Parses an absolute http(s) URL with a host, and without a query or a
-- fragment.
parseHttpUrl :: String -> Either String URI
parseHttpUrl input = case parseAbsoluteURI input of
  Nothing -> Left $ show input ++ " is not a valid absolute URL"
  Just uri
    | uriScheme uri `notElem` ["http:", "https:"] -> Left $ show input ++ " must start with http:// or https://"
    | not (hasHost uri) -> Left $ show input ++ " must contain a host"
    | not (hasValidPort uri) -> Left $ show input ++ " has an invalid port, it must be between 1 and 65535"
    | not (null $ uriQuery uri) || not (null $ uriFragment uri) -> Left $ show input ++ " must not contain a query or a fragment"
    | otherwise -> Right uri
  where
    hasHost uri = isJust $ uriAuthority uri >>= nonEmpty . uriRegName
    nonEmpty "" = Nothing
    nonEmpty s = Just s

    -- `network-uri` follows the URI RFC (3986), which allows a port with any
    -- number of digits, so we check it with the same rules as the port options.
    hasValidPort uri = case uriPort <$> uriAuthority uri of
      Just (':' : digits) -> isRight $ parsePort digits
      _ -> True
