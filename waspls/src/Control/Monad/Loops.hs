module Control.Monad.Loops
  ( untilM,
  )
where

-- | Analogue of 'until'. @'untilM' p f b@ yields the result of applying @f@
-- until @p@ is true.
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM predicate f base
  | predicate base = return base
  | otherwise = f base >>= untilM predicate f
