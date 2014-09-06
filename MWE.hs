{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeNaturals #-}
{-# LANGUAGE UndecidableInstances #-}

module MWE where

import Control.Applicative
import GHC.TypeLits

data SMatrix :: Nat -> Nat -> * -> * where
    Zero :: SMatrix i j a
    Identity :: SMatrix i i a
    Block :: SMatrix i k a -> SMatrix j k a -> SMatrix i l a -> SMatrix j l a -> SMatrix (i+j) (k+l) a
    Dense :: a -> SMatrix i j a

instance Functor (SMatrix i j) where
  fmap f (Dense x) = Dense (f x)
  fmap f (Block x11 x12 x21 x22) = Block
      (fmap f x11) (fmap f x12)
      (fmap f x21) (fmap f x22)

shapeOf :: SMatrix i j a -> SMatrix i j b -> SMatrix i j a
shapeOf a b = a

instance Applicative (SMatrix i j) where
  pure = Dense
  (Dense f) <*> (Dense x) = Dense (f x)
  (Dense f) <*> (Block x11 x12 x21 x22) = Block
      (fmap f x11) (fmap f x12)
      (fmap f x21) (fmap f x22)
  (Block f11 f12 f21 f22) <*> (Block x11 x12 x21 x22) = Block
      (f11 <*> x11) (f12 <*> x12)
      (f21 <*> x21) (f22 <*> x22)


