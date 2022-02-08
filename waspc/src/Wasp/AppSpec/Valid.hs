module Wasp.AppSpec.Valid
  ( Valid,
    (<$^>),
    fromValid,
  )
where

import Wasp.AppSpec.Valid.Internal

fromValid :: Valid a -> a
fromValid (Valid a) = a

(<$^>) :: (a -> b) -> Valid a -> b
(<$^>) f = f . fromValid

infixl 4 <$^> -- Same fixity as `<$>`.
