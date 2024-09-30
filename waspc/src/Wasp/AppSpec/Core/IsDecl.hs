{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.AppSpec.Core.IsDecl
  ( IsDecl (..),
  )
where

import Data.Typeable (Proxy (Proxy), Typeable, typeRep)

class (Typeable a) => IsDecl a where
  -- | Returns the name of the Wasp declaration type.
  -- For example, for a Wasp declaration `page MyPage {...}`, this could return "page".
  -- By default implementation, it returns the name of the Haskell type used to represent
  -- the declaration.
  declTypeName :: String
  declTypeName = show $ typeRep (Proxy :: Proxy a)
