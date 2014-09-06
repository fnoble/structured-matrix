{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Numeric.StructuredMatrix where

import Control.Applicative
import GHC.TypeLits
import qualified Numeric.LinearAlgebra as H

data MatrixI a (n::Nat) (m::Nat) = MatrixI a
  deriving (Eq, Show)

class Matrix a where
  mmmult :: a -> a -> a
  fromBlocks :: a -> a -> a -> a -> a
  zero :: a
  identity :: a

(.##.) :: (Matrix a) => MatrixI a i j -> MatrixI a j k -> MatrixI a i k
(MatrixI a) .##. (MatrixI b) = MatrixI (a `mmmult` b)

id' :: (Num a, H.Element a, SingI n) => Sing n -> MatrixI (H.Matrix a) n n
id' s = MatrixI $ H.ident $ fromInteger $ fromSing s

ident :: (Num a, H.Element a, SingI n) => MatrixI (H.Matrix a) n n
ident = withSing id'

data SMatrix a = Zero
               | Identity
               | Block a a a a
               | Dense a
  deriving (Eq)

instance Functor SMatrix where
  fmap f (Dense x) = Dense (f x)
  fmap f (Block x11 x12 x21 x22) = Block (f x11) (f x12) (f x21) (f x22)

instance Applicative SMatrix where
  pure = Dense
  (Dense f) <*> (Dense x) = Dense (f x)
  (Dense f) <*> (Block x11 x12 x21 x22) = Block (f x11) (f x12) (f x21) (f x22)
  (Block f11 f12 f21 f22) <*> (Dense x) = Block (f11 x) (f12 x) (f21 x) (f22 x)
  (Block f11 f12 f21 f22) <*> (Block x11 x12 x21 x22) = Block (f11 x11)
                                                              (f12 x12)
                                                              (f21 x21)
                                                              (f22 x22)

instance (Num a, H.Element a, H.Product a) => Matrix (H.Matrix a) where
  mmmult = (H.<>)
  fromBlocks x11 x12 x21 x22 = H.fromBlocks [[x11, x12],
                                             [x21, x22]]
  zero = H.diag $ H.constant 0 1
  identity = H.ident 1

fromStructured (Block x11 x12 x21 x22) =
  fromBlocks x11 x12 x21 x22
fromStructured Zero = zero
fromStructured Identity = identity
fromStructured (Dense x) = x

instance (Matrix a) => Matrix (SMatrix a) where
  Zero `mmmult` x = Zero
  x `mmmult` Zero = Zero
  Identity `mmmult` x = x
  x `mmmult` Identity = x
  x `mmmult` y = mmmult <$> x <*> y

  fromBlocks x11 x12 x21 x22 = Block (fromStructured x11) (fromStructured x12)
                                     (fromStructured x21) (fromStructured x22)

  zero = Zero
  identity = Identity

-- Num operations defined elementwise
instance (Num a) => Num (SMatrix a) where
  Zero + x = x
  x + Zero = x
  {-Identity + x = x + fromInteger 1-}
  {-x + Identity = Identity + x-}
  x + y = (+) <$> x <*> y

  Zero * x = Zero
  x * Zero = Zero
  Identity * x = x
  x * Identity = x
  x * y = (*) <$> x <*> y

  negate Zero = Zero
  negate Identity = fromInteger (-1) * Identity
  negate x = negate <$> x

  abs Zero = Zero
  abs Identity = Identity
  abs x = abs <$> x

  signum Zero = fromInteger 0
  signum Identity = fromInteger 1
  signum x = signum <$> x

  fromInteger x = Dense (fromInteger x)


-- Test values
m = (3H.><3) [1..] :: H.Matrix Double
n = (3H.><3) [10..] :: H.Matrix Double
a = Block Identity Zero Zero (Dense n)
b = Dense (Dense m)

