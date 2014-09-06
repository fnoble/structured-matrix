{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Foo where

import Control.Applicative
import GHC.TypeLits
import qualified Numeric.LinearAlgebra as H

data SMatrix :: * -> Nat -> Nat -> * where
  Zero :: SMatrix a i j
  Identity :: SMatrix a i i
  Block :: (KnownNat i, KnownNat j, KnownNat k, KnownNat l) =>
           SMatrix a i k -> SMatrix a j k ->
           SMatrix a i l -> SMatrix a j l ->
             SMatrix a (i+j) (k+l)
  Dense :: a -> SMatrix a i j
