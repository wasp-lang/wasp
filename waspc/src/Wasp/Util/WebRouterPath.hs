module Wasp.Util.WebRouterPath
  ( Segment (StaticSegment, ParamSegment),
    StaticSegment (RequiredStaticSegment, OptionalStaticSegment),
    ParamSegment (RequiredParamSegment, OptionalParamSegment),
    getRouteSegments,
  )
where

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)

data Segment = StaticSegment StaticSegment | ParamSegment ParamSegment deriving (Show, Eq)

data StaticSegment = RequiredStaticSegment StaticSegmentValue | OptionalStaticSegment StaticSegmentValue deriving (Show, Eq)

data ParamSegment = RequiredParamSegment ParamName | OptionalParamSegment ParamName deriving (Show, Eq)

type StaticSegmentValue = String

type ParamName = String

getRouteSegments :: String -> [Segment]
getRouteSegments = map parseSegment . splitOn "/"
  where
    parseSegment :: String -> Segment
    parseSegment "*" = ParamSegment $ RequiredParamSegment "splat"
    parseSegment (':' : xs) =
      ParamSegment $
        if isSegmentOptional xs
          then OptionalParamSegment (take (length xs - 1) xs)
          else RequiredParamSegment xs
    parseSegment x =
      StaticSegment $
        if isSegmentOptional x
          then OptionalStaticSegment x
          else RequiredStaticSegment x

isSegmentOptional :: String -> Bool
isSegmentOptional = isSuffixOf "?"
