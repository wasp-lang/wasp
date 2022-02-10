{-# LANGUAGE PatternSynonyms #-}

module Wasp.AppSpec.Valid
  ( -- |
    -- Idea behind @Valid@ is that you use it to mark any data structure as valid,
    -- by using @MakeValid@ constructor from "Valid.Internal", normally at the end of
    -- your validation logic.
    -- So, if you have some @data Foo = ...@, your validation logic would look smth like
    -- @validateFoo :: Foo -> Valid Foo@.
    -- Then, what @Valid@ does, is allow you to manipulate its contents, or to take them out of @Valid@.
    Valid (Valid),
    fromValid,
    ($^),
    propagateValid,
    (<$^>),
    (<$^^>),
  )
where

import Wasp.AppSpec.Valid.Internal (Valid (..))

-- By defining @pattern Valid@, others can pattern match on `Valid`, but they can't construct it.

pattern Valid :: a -> Valid a
pattern Valid a <- MakeValid a

-- NOTE: This is needed by compiler to know that Valid is the only pattern in the pattern set,
--   so it doesn't warn about missing patterns.
{-# COMPLETE Valid #-}

fromValid :: Valid a -> a
fromValid (MakeValid a) = a

($^) :: (a -> b) -> Valid a -> b
($^) f = f . fromValid

infixl 4 $^ -- Same fixity as `<$>`.

(<$^>) :: (Functor f) => (a -> f b) -> Valid a -> f (Valid b)
(<$^>) f (MakeValid x) = fmap MakeValid (f x)

infixl 4 <$^> -- Same fixity as `<$>`.

(<$^^>) :: (Functor f1, Functor f2) => (a -> f1 (f2 b)) -> Valid a -> f1 (f2 (Valid b))
(<$^^>) f (MakeValid x) = (fmap . fmap) MakeValid (f x)

infixl 4 <$^^> -- Same fixity as `<$>`.

propagateValid :: (Functor f) => Valid (f a) -> f (Valid a)
propagateValid (MakeValid xs) = MakeValid <$> xs
