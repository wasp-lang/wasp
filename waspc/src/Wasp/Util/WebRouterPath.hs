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
    parseSegment "*" = ParamSegment $ RequiredParamSegment "*"
    parseSegment (':' : paramName) =
      ParamSegment $
        if isSegmentOptional paramName
          then OptionalParamSegment $ init paramName
          else RequiredParamSegment paramName
    parseSegment segmentValue =
      StaticSegment $
        if isSegmentOptional segmentValue
          then OptionalStaticSegment segmentValue
          else RequiredStaticSegment segmentValue

isSegmentOptional :: String -> Bool
isSegmentOptional = isSuffixOf "?"
