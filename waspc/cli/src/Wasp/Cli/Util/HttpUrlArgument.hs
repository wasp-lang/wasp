module Wasp.Cli.Util.HttpUrlArgument
  ( httpUrlOption,
    parseHttpUrl,
  )
where

import Data.Maybe (isJust)
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)
import qualified Options.Applicative as Opt

httpUrlOption :: String -> String -> Opt.Parser (Maybe URI)
httpUrlOption optionName helpText =
  Opt.optional $
    Opt.option
      (Opt.str >>= either Opt.readerError return . parseHttpUrl)
      ( Opt.long optionName
          <> Opt.metavar "URL"
          <> Opt.help helpText
      )

-- | Parses an absolute http(s) URL with a host, and without a query or a
-- fragment, i.e. a URL at which something can be reached over the web.
parseHttpUrl :: String -> Either String URI
parseHttpUrl input = case parseAbsoluteURI input of
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
