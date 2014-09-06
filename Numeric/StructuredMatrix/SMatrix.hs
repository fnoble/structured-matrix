{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}

module Numeric.StructuredMatrix.SMatrix where

import Control.Applicative
import GHC.TypeLits
import qualified Numeric.LinearAlgebra as H

class Matrix a where
  mmmult :: a -> a -> a
  fromBlocks :: a -> a -> a -> a -> a
  zero :: Int -> Int -> a
  identity :: Int -> a
  rows :: a -> Int
  cols :: a -> Int

data SMatrix :: Nat -> Nat -> * -> * where
  Zero :: SMatrix i j a
  Identity :: SMatrix i i a
  Block :: (KnownNat i, KnownNat j, KnownNat k, KnownNat l) =>
           SMatrix k i a -> SMatrix k j a ->
           SMatrix l i a -> SMatrix l j a ->
             SMatrix (k+l) (i+j) a
  Dense :: a -> SMatrix i j a

instance Functor (SMatrix i j) where
  -- Note: fmap is a partial function over SMatricies as the
  -- desired behaviour sometimes depends on f
  fmap f (Dense x) = Dense (f x)
  fmap f (Block x11 x12 x21 x22) = Block
    (fmap f x11) (fmap f x12)
    (fmap f x21) (fmap f x22)

instance (Matrix a) => Applicative (SMatrix i j) a where
  pure = Dense
  (Dense f) <*> (Dense x) = Dense (f x)
  (Dense f) <*> (Block x11 x12 x21 x22) = Block
    (fmap f x11) (fmap f x12)
    (fmap f x21) (fmap f x22)
  (Dense f) <*> x = Dense (f (fromStructured x))
  -- Doesn't compile - the problem is the type signature only constrains the
  -- size of the total matrix to match but doesn't constrain the position of
  -- the 'split' into blocks to be the same. Not sure how or if to fix.
  {-
  (Block f11 f12 f21 f22) <*> (Block x11 x12 x21 x22) = Block
    (f11 <*> x11) (f12 <*> x12)
    (f21 <*> x21) (f22 <*> x22)
  -}

instance (Num a, H.Element a, H.Product a) => Matrix (H.Matrix a) where
  mmmult = (H.<>)
  fromBlocks x11 x12 x21 x22 = H.fromBlocks [[x11, x12],
                                             [x21, x22]]
  zero i j = H.buildMatrix i j $ \_ -> fromIntegral 0
  identity = H.ident
  rows = H.rows
  cols = H.cols

data NatProxy :: Nat -> *

{-fromDense' :: (Matrix a, KnownNat i, KnownNat j) => proxy i -> proxy j -> a -> SMatrix i j a-}
{-fromDense' i j m = Dense m-}
{-[>fromDense' = _<]-}

{-fromDense :: (Matrix a) => a -> (forall i j . SMatrix i j a)-}
{-fromDense m = f (someNatVal (fromIntegral (rows m)))-}
                {-(someNatVal (fromIntegral (cols m))) m-}
            {-where f :: (Matrix a) => Maybe SomeNat -> Maybe SomeNat -> a -> SMatrix i j a-}
                  {-f (Just (SomeNat i)) (Just (SomeNat j)) m = fromDense' i j m-}

{-withDense :: (Matrix a) => (SMatrix i j a -> b) -> a -> b-}
{-withDense f a = f (fromDense (rows a) (cols a) a)-}
              {-where f :: (KnownNat i) => Int -> proxy i-}
                    {-f x = case someNatVal (fromIntegral x) of-}
                            {-(Just (SomeNat i)) -> i-}
                    {-fromDense :: (Matrix a, KnownNat i, KnownNat j) =>-}
                                    {-proxy i -> proxy j -> a -> SMatrix i j a-}
                    {-fromDense i j m = Dense m-}

fromStructured :: (Matrix a, KnownNat i, KnownNat j) => SMatrix i j a -> a
fromStructured = f undefined undefined
  where f :: (Matrix a, KnownNat i, KnownNat j) =>
               NatProxy i -> NatProxy j -> SMatrix i j a -> a
        f i j a = fromStructured' (fromInteger (natVal i))
                                  (fromInteger (natVal j)) a

fromStructured' :: (Matrix a, KnownNat n, KnownNat m) =>
                   Int -> Int -> SMatrix n m a -> a
fromStructured' i j (Dense m) = m
fromStructured' i j Zero = zero i j
fromStructured' i j Identity = identity i
fromStructured' i j (Block x11 x12 x21 x22) = fromBlocks
  (fromStructured x11) (fromStructured x12)
  (fromStructured x21) (fromStructured x22)


toDense :: (Matrix a, KnownNat i, KnownNat j) => SMatrix i j a -> SMatrix i j a
toDense m = Dense (fromStructured m)

-- Num operations defined elementwise
instance (Num a) => Num (SMatrix i j a) where
  Zero + x = x
  x + Zero = x
  x + y = (+) <$> x <*> y

  Zero * x = Zero
  x * Zero = Zero
  Identity * x = x
  x * Identity = x
  x * y = (*) <$> x <*> y

  negate Zero = Zero
  negate x = negate <$> x

  abs Zero = Zero
  abs Identity = Identity
  abs x = abs <$> x

  signum Zero = fromInteger 0
  signum Identity = fromInteger 1
  signum x = signum <$> x

  fromInteger x = Dense (fromInteger x)

{-
fromStructured s@Identity = (withSing (withSing fromStructured')) s
fromStructured (Dense x) = x

instance (Matrix a, KnownNat i, KnownNat j) => Matrix (SMatrix i j a) where
  Zero `mmmult` x = Zero
  x `mmmult` Zero = Zero
  Identity `mmmult` x = x
  x `mmmult` Identity = x
  x `mmmult` y = mmmult <$> x <*> y

  fromBlocks x11 x12 x21 x22 = Block (fromStructured x11) (fromStructured x12)
                                     (fromStructured x21) (fromStructured x22)

  zero i j = Zero
  identity i = Identity

(.##.) :: (Matrix a, KnownNat i, KnownNat j, KnownNat k) => SMatrix i j a -> SMatrix j k a -> SMatrix i k a
{-(.##.) :: (Matrix a) => SMatrix i j a -> SMatrix j k a -> SMatrix i k a-}
Zero .##. x = Zero
x .##. Zero = Zero
Identity .##. Identity = Identity
Identity .##. (Dense x) = Dense x
(Dense x) .##. Identity = Dense x
{-x .##. y = (.##.) <$> x <*> y-}

-}

-- Test values
m = (3H.><3) [1..] :: H.Matrix Double
n = (3H.><3) [10..] :: H.Matrix Double
a = Block (Identity :: SMatrix 4 4 (H.Matrix Double)) Zero
          Zero ((Dense n) :: SMatrix 3 3 (H.Matrix Double))
b = Dense (Dense m)

