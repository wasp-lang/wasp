module Wasp.Util.WebRouterPath where

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Param = Optional String | Required String deriving (Show, Eq)

-- TODO: upgrade to work with React Router v6: https://reactrouter.com/en/main/route/route#splats
extractPathParams :: String -> [Param]
extractPathParams = mapMaybe parseParam . splitOn "/"
  where
    parseParam :: String -> Maybe Param
    parseParam (':' : xs) =
      Just $
        if "?" `isSuffixOf` xs
          then Optional (take (length xs - 1) xs)
          else Required xs
    parseParam _ = Nothing
