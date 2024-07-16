module Wasp.Psl.Generator.Enum
  ( generateEnumBody,
  )
where

import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import Wasp.Psl.Generator.Attribute (generateAttribute)
import Wasp.Psl.Generator.Common (PslSource)

generateEnumBody :: [Psl.Enum.Element] -> PslSource
generateEnumBody values = unlines $ map ("  " ++) $ generateEnumElement <$> values

generateEnumElement :: Psl.Enum.Element -> PslSource
generateEnumElement (Psl.Enum.ElementValue value attrs) =
  value ++ concatMap ((" " ++) . generateAttribute) attrs
generateEnumElement (Psl.Enum.ElementBlockAttribute attribute) = "@" ++ generateAttribute attribute
