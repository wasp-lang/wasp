module Util.Control.Monad
  ( foldMapM',
  )
where

import Data.List (foldl')

-- | Analogous to "Prelude.foldMap'", except that its result is encapsulated in a
-- monad.
--
-- @
-- foldMapM f [x1, x2, ..., xn] ==
--   do
--     a1 <- f x1
--     a2 <- f x2
--     ...
--     an <- f xn
--     return $ mempty <> a1 <> a2 <> ... <> an
-- @
--
-- __Examples__
--
-- >>> import Data.Monoid
-- >>> getSum <$> foldMapM' (\n -> if n > 3 then Right n else Left n) (map Sum [4,7,5,6])
-- Right 22
foldMapM' :: (Foldable t, Monad m, Monoid s) => (a -> m s) -> t a -> m s
foldMapM' f = foldl' (\ms a -> ms >>= \s -> (s <>) <$> f a) $ pure mempty
