module Util where

import Control.Applicative (liftA2)

infixr 5 <++>

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)
