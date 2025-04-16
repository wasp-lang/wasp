module Wasp.Generator.Valid.PackageJsonValidationContext
  ( PackageJsonValidationContext (..),
  )
where

data PackageJsonValidationContext = PackageJsonValidationContext
  { isTailwindUsed :: Bool
  }
