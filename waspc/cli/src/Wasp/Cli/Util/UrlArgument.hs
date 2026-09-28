module Wasp.Cli.Util.UrlArgument
  ( urlOption,
    parseUrl,
  )
where

import Data.Maybe (isJust)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import qualified Options.Applicative as Opt

urlOption :: String -> String -> Opt.Parser (Maybe URI)
urlOption optionName helpText =
  Opt.optional $
    Opt.option
      (Opt.str >>= either Opt.readerError return . parseUrl)
      ( Opt.long optionName
          <> Opt.metavar "URL"
          <> Opt.help helpText
      )

-- | Parses a URL at which something can be reached over the web: an absolute
-- http(s) URL with a host, and without a query or a fragment.
parseUrl :: String -> Either String URI
parseUrl input = case parseAbsoluteURI input of
  Nothing -> Left $ show input ++ " is not a valid absolute URL"
  Just uri
    | uriScheme uri `notElem` ["http:", "https:"] -> Left $ show input ++ " must start with http:// or https://"
    | not (hasHost uri) -> Left $ show input ++ " must contain a host"
    | not (null $ uriQuery uri) || not (null $ uriFragment uri) -> Left $ show input ++ " must not contain a query or a fragment"
    | otherwise -> Right uri
  where
    hasHost uri = isJust $ uriAuthority uri >>= nonEmpty . uriRegName
    nonEmpty "" = Nothing
    nonEmpty s = Just s
