module Wasp.Wasp.JsCode
  ( JsCode (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.Text (Text)

newtype JsCode = JsCode Text deriving (Show, Eq)

-- TODO(matija): Currently generator is relying on this implementation, which is not
-- ideal. Ideally all the generation logic would be in the generator. But for now this was
-- the simplest way to implement it.
instance ToJSON JsCode where
  toJSON (JsCode code) = toJSON code
