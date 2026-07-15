{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Route
  ( Route (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Page

data Route = Route
  { path :: String,
    -- TODO: In the future we might want to add other types of targets, for example another Route.
    --   For that the best solution is probably to implement sum types (https://github.com/wasp-lang/wasp/issues/381).
    to :: Ref Page,
    lazy :: Maybe Bool,
    -- | Concrete static paths to prerender at build time (empty when
    -- prerendering is disabled). The public @prerender: true@ shorthand is
    -- normalized to @[routePath]@ by the spec mappers (in the TS @spec@
    -- package), so by the time it reaches here it is always a list of paths.
    prerender :: [String]
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Route

instance Inspectable Route where
  inspect route =
    InspectionEntry
      "Routes"
      ( [ ("Path", path route),
          ("Destination", refName (to route)),
          ("Loading", if lazy route == Just False then "Eager" else "Lazy")
        ]
          ++ [("Prerender", "Enabled") | not $ null $ prerender route]
      )
      : [ InspectionEntry
            "Prerendered routes"
            [("Route", prerenderPath)]
        | prerenderPath <- prerender route
        ]
