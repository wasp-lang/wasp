module Wasp.Parser.App
  ( app,
  )
where

import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)
import Wasp.Lexer
import qualified Wasp.Lexer as L
import Wasp.Parser.Common
import qualified Wasp.Wasp.App as App

-- | A type that describes supported app properties.
data AppProperty
  = Title !String
  | Favicon !String
  | Head [String]
  deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties =
  commaSep1 $
    appPropertyTitle
      <|> appPropertyFavicon
      <|> appPropertyHead

appPropertyTitle :: Parser AppProperty
appPropertyTitle = Title <$> waspPropertyStringLiteral "title"

appPropertyFavicon :: Parser AppProperty
-- TODO(matija): 'fav.png' currently does not work because of '.'. Support it.
appPropertyFavicon = Favicon <$> waspPropertyStringLiteral "favicon"

appPropertyHead :: Parser AppProperty
appPropertyHead = Head <$> waspProperty "head" (L.brackets $ L.commaSep1 L.stringLiteral)

-- TODO(matija): unsafe, what if empty list?
getAppTitle :: [AppProperty] -> String
getAppTitle ps = head $ [t | Title t <- ps]

getAppHead :: [AppProperty] -> Maybe [String]
getAppHead ps = listToMaybe [hs | Head hs <- ps]

-- | Top level parser, parses App.
app :: Parser App.App
app = do
  (appName, appProps) <- waspElementNameAndClosureContent reservedNameApp appProperties

  return
    App.App
      { App.appName = appName,
        App.appTitle = getAppTitle appProps,
        App.appHead = getAppHead appProps
        -- TODO(matija): add favicon.
      }
