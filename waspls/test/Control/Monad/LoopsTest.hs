{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.LoopsTest where

import Control.Monad.Loops
import Test.Tasty.Hspec

data Identity a = Identity {runIdentity :: a} deriving (Functor)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

spec_untilM :: Spec
spec_untilM = do
  it "Is identical to until with the Identity monad" $ do
    runIdentity (untilM (== 0) (return . pred) 10) `shouldBe` until (== 0) pred (10 :: Int)

  it "Works with Maybe monad" $ do
    let test n = untilM (== 0) (\x -> if x < 0 then Nothing else return (pred x)) n
    test (10 :: Int) `shouldBe` Just 0
    test (-2 :: Int) `shouldBe` Nothing
