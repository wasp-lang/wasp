module Wasp.Psl.Ast.Type
  ( Type (..),
  )
where

import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.Model (Body)

data Type
  = Type
      Name
      Body
  deriving (Show, Eq)
