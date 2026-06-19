module Wasp.Util.WebRouterPath
  ( Segment (StaticSegment, ParamSegment),
    StaticSegment (RequiredStaticSegment, OptionalStaticSegment),
    ParamSegment (RequiredParamSegment, OptionalParamSegment),
    getRouteSegments,
    doesConcretePathMatchRoutePattern,
  )
where

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)

data Segment
  = StaticSegment StaticSegment
  | ParamSegment ParamSegment
  deriving (Show, Eq)

data StaticSegment
  = RequiredStaticSegment StaticSegmentValue
  | OptionalStaticSegment StaticSegmentValue
  deriving (Show, Eq)

data ParamSegment
  = RequiredParamSegment ParamName
  | OptionalParamSegment ParamName
  deriving (Show, Eq)

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

    isSegmentOptional = isSuffixOf "?"

-- | Returns whether a concrete path (e.g. @\/blog\/intro@) is matched by a
-- route's path pattern (e.g. @\/blog\/:slug@).
--
-- The route pattern may contain dynamic params (@:param@), optional segments
-- (trailing @?@) and a trailing splat (@*@). The concrete path is expected to
-- be fully static. This is used to check that a prerender path actually belongs
-- to the route it is declared on.
doesConcretePathMatchRoutePattern :: String -> String -> Bool
doesConcretePathMatchRoutePattern routePattern concretePath =
  matchSegments (getRouteSegments routePattern) (splitOn "/" concretePath)
  where
    matchSegments :: [Segment] -> [String] -> Bool
    matchSegments [] [] = True
    -- A trailing splat matches any number (including zero) of remaining segments.
    matchSegments [ParamSegment (RequiredParamSegment "*")] _ = True
    matchSegments [] _ = False
    matchSegments (segment : segments) concreteSegments = case segment of
      ParamSegment (OptionalParamSegment _) ->
        consume (not . null) segments concreteSegments
          || matchSegments segments concreteSegments
      StaticSegment (OptionalStaticSegment value) ->
        consume (== dropOptionalMarker value) segments concreteSegments
          || matchSegments segments concreteSegments
      ParamSegment (RequiredParamSegment _) ->
        consume (not . null) segments concreteSegments
      StaticSegment (RequiredStaticSegment value) ->
        consume (== value) segments concreteSegments

    -- Consume one concrete segment if it satisfies the predicate, then match
    -- the remaining pattern segments against the rest.
    consume :: (String -> Bool) -> [Segment] -> [String] -> Bool
    consume _ _ [] = False
    consume isMatch segments (concreteSegment : rest) =
      isMatch concreteSegment && matchSegments segments rest

    dropOptionalMarker = init -- strips the trailing '?' kept by getRouteSegments
