---
title: "Tutorial: `forall` in Haskell"
authors: [martinsos]
tags: [haskell, tutorial]
---

import useBaseUrl from '@docusaurus/useBaseUrl';

--------

Find out what Haskell's `forall` is all about.

<!--truncate-->

You might have seen `forall` being used in Haskell like this:
```haskell
f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
```
or
```hs
liftPair :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
```
or
```hs
data Showable = forall s. (Show s) => Showable s
```

`forall` is something called "type quantifier", and it gives extra meaning to polymorphic type signatures (e.g. `:: a`, `:: a -> b`, `:: a -> Int`, ...).

While normaly `forall` plays a role of the "universal quantifier", it can also play a role of the "existential quantifier" (depends on the situation).

What does all this mean and how can `forall` be used in Haskell? Read on to find out!

NOTE: we assume you are comfortable with basic polymorphism in Haskell.

## Quick math/logic reminder

In mathematical logic, we have

- **[universal quantifier](https://en.wikipedia.org/wiki/Universal_quantification)**
  - symbol: ∀x
  - interpretation: "for all", "given any"
  - example: `∀x P(x)` means "for all x predicate P(x) is true".
- **[existential quantifier](https://en.wikipedia.org/wiki/Existential_quantification)**
  - symbol: ∃x
  - interpretation: "there exists", "there is at least one", "for some"
  - example: `∃x P(x)` means "there is some x for which predicate P(x) is true".

## Vanilla Haskell (no extensions)

In Haskell, all polymorphic type signatures are considered to be implicitly prefixed with `forall`.

Therefore, if you have
```hs
f :: a -> a
g :: a -> (a -> b) -> b
```
it is really the same as
```hs
f :: forall a. a -> a
g :: forall a b. a -> (a -> b) -> b
```

What `forall` here does is play the role of universal quantifier.
For function `f`, it means it is saying "for all types, this function takes that type and returns the same type.".
Other way to put it would be "this funtion can be called with value of any type as its first argument, and it will return the value of that same type".

Since `forall` is already implicit, writing it explicitly doesn't really do anything!

Not only that, but without any extensions, you can't even write `forall` explicitly, you will get a syntax error, since `forall` is not a keyword in Haskell.

So what is the purpose of `forall` then? Well, obviously to be used with extensions :)!

The simplest extension is `ExplicitForAll`, which allows you to explicitly write `forall` (as we did above).
This is not useful on its own though, since as we said above, explicitly writing `forall` doesn't change anything, it was already implicitly there.

However, there are other extensions that make use of `forall` keyword, like: `ScopedTypeVariables`, `RankNTypes`, `ExistentialQuantification`.
All these extensions automatically enable `ExplicitForAll` extension, which means you don't need to enable it yourself when using any of these.
There is also `TypeApplications` extension which interacts with `forall` and in that case you might want to use `ExplicitForAll` with it.

Since `forall` is useful only when used with extensions, let's take a look at how it is used in each one of those!

## `forall` and extension [ScopedTypeVariables](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#lexically-scoped-type-variables)

`ScopedTypeVariables` enables lexical scoping of type variables by explicitly introducing them with `forall`.

Let's take a look at the following example:
```hs
f :: [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
```

This code does not compile, because compiler can't match type of `ys` with the return type of `f`.
Why though, when they are both `[a]`? Well, that is because that is not the same `a`!
Try changing `ys :: [a]` to `ys :: [b]` and you will get the exact same error,
because it is exactly the same code -> `a` in `ys :: [a]` and `a` in `f :: [a] -> [a]` are different `a`s and there is no connection between them.
`a` in `ys :: [a]` stands for "any type", not for "that type that is reffered to with `a` in the type signature above".

This is where `ScopedTypeVariables` comes in:
```hs
{-# LANGUAGE ScopedTypeVariables #-}

f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs
```

`forall` now gets special powers: the type variables bound by a forall (in our case `a`) scope over the entire definition
of the accompanying value declaration (in this case definition of `f`).

This means that any mention of type `a` in the definition of `f` now refers to that `a` from the type signature of `f`,
which is exactly what we needed, and the code example above now compiles.

## `forall` and extension [RankNTypes](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#arbitrary-rank-polymorphism)

Basically, what `RankNTypes` does is enable you to use `forall` nested in type signatures, so that it does not apply to the whole type signature but just the part of it.

This enables some cool things that you were not able to do to before, for example you can specify that your function takes a polymorphic function as an argument.

Take a look at this example:
```hs
foo :: (forall a. a -> a) -> (Char, Bool)    -- We can do this only with RankNTypes.

bar :: forall a. ((a -> a) -> (Char, Bool))  -- This is usual stuff, we don't need RankNTypes for it. Actually we can even drop `forall` since it is implicit.
```
In `foo`, `forall` is applied only to the first argument of `foo`, which is `a -> a`, and not to the rest of the `f`'s type signature. This can be done only with `RankNTypes` extension.
`bar` on the other hand has `forall` applied to the whole signature, and we could have even ommited this `forall` since it would be there implicitly anyway.

Now, what does this mean? If we now have `specificFunc :: Int -> Int` and `polymorphicFunc :: a -> a`, `foo polymorphicFunc` will compile, while `foo specificFunc` will not! On the other hand both `bar specificFunc` and `bar polymorphicFunc` will compile.

This is because we specified, with `forall`, that `foo` needs a polymorphic function (function that takes value of **any** type and returns value of that same type) as the first argument, so we can't pass it a function like `specificFunc` that works only for `Int` -> such function is too specific.
On the other hand, `bar` needs a function that takes value of **some** type and returns the value of that same type, so `specificFunc` is completely fine since it works only with `Int`, while `polymorphicFunc` is also ok although it is more general than what is needed, since compiler can easily specialize it. 

Another example is `liftPair` function:
```hs
liftPair :: (forall x. x -> f x) -> (a, b) -> (f a, f b)
liftPair func (y, z) = (func y, func z)
```
```
>> liftPair (:[]) (1, "a")
([1], ["a"])
```
`liftPair` takes polymorphic function and applies it to both values in the pair.
There would be no way to write its type signature without using `RankNTypes`.

## `forall` and extension [ExistentialQuantification](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#existentially-quantified-data-constructors)

`ExistentialQuantification` enables us to use `forall` in the type signature of data constructors.

This is useful because it enables us to define heterogeneous data types, which then allows us to store different types in a single data collection (which normally you can't do in Haskell, e.g. you can't have different types in a list).

For example, if we have
```hs
data Showable = forall s. (Show s) => Showable s
```
now we can do
```hs
someShowables :: [Showable]
someShowables = [Showable "Hi", Showable 5, Showable (1, 2)]

printShowables :: [Showable] -> IO ()
printShowables ss = mapM_ (\(Showable s) -> print s) ss

main :: IO ()
main = printShowables someShowables
```

In this example this allowed us to create a heterogeneous list, but only thing we can do with the contents of it is show them.

What is interesting is that in this case, `forall` plays the role of an existential quantifier (therefore the name of extension, `ExistentialQuantification`), unlike the role of universal quantifier it normally plays.

### GADTs

Alternative approach to `ExistentialQuantification` is to use the `GADTs` extension, like this:
```hs
{-# LANGUAGE GADTs #-}
data Showable where
  Showable :: (Show s) => s -> Showable
```

In this case `forall` is not needed, as it is implicit.


## `forall` and extension [TypeApplications](https://ghc.readthedocs.io/en/latest/glasgow_exts.html#visible-type-application)

`TypeApplications` does not change how `forall` works like the extensions above do, but it does have an interesting interaction with `forall`, so we will mention it here.

`TypeApplications` allows you to specify values of types variables in a type.

For example, you can do `show (read @Int "5")` to specify that `"5"` should be interpreted as an `Int`.
`read` has type signature `:: Read a => String -> a`, so what `@Int` does is say that that `a` in the type signature is `Int`.
Therefore, `read @Int :: String -> Int`.

How does `forall` come into play here?

Well, if an identifier’s type signature does not include an explicit `forall`, the type variable arguments appear in the left-to-right order in which the variables appear in the type. So, `foo :: Monad m => a b -> m (a c)` will have its type variables ordered as `m`, `a`, `b`, `c`, and type applications will happen in that order: if we have `foo @Maybe @Either`, `@Maybe` will apply to `m` while `@Either` will apply to `a`. However, if you want to force a different order, for example `a`, `b`, `c`, `m`, so that `@Maybe` in `foo @Maybe @Either` applies to `a`, you can refactor the signature as `foo :: forall a b c m. Monad m => a b -> m (a c)`, and now order of type variables in `forall` will be used when doing type applications!

This will require you to enable `ExplicitForAll` extension, if it is not already enabled.

# Conclusion

This document should give a fair idea of how `forall` is used and what can be done with it, but it doesn't go into much depth or cover all of the ways `forall` is used in Haskell.

For more in-detail explanations and further investigation, here is a couple of useful resources:
- Great SO question/answers: https://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do .
- Docs on extensions, which in details describe how each of them works: https://ghc.readthedocs.io/en/latest/glasgow_exts.html .
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html .
- Article about `ST` monad that also explains a lot about `forall`: https://dl.acm.org/doi/10.1145/178243.178246 . 

This blog post originated from the [notes I wrote in wasp-lang/haskell-handbook](https://github.com/wasp-lang/haskell-handbook/blob/35f14951b17f2174cbc269628bea157b6698a9e4/forall.md).
