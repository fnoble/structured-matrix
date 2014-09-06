{-|
Module      : Numeric.StructuredMatrix.PrettyPrint
Description : Pretty printing functions for Structured Matrices
Copyright   : (c) Fergus Noble, 2014
License     : BSD3
Maintainer  : fergusnoble@gmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE GADTs #-}

module Numeric.StructuredMatrix.PrettyPrint where

import Numeric.StructuredMatrix.SMatrix hiding (rows, cols)
import Text.PrettyPrint.Boxes

-- |Convert a multi-line string into a Box
multilineBox :: String -> Box
multilineBox s = foldr (//) nullBox $ map text $ lines s

-- |Represent anything that implements Show as a Box
showBox :: (Show a) => a -> Box
showBox x = multilineBox $ show x

-- |Assemble four Boxes in a grid with center alignement
grid :: Box -> Box -> Box -> Box -> Box
grid b11 b12 b21 b22 =
  vsep 2 center1
    [hsep 4 center1 [alc maxHeightRow1 maxWidthCol1 b11,
                     alc maxHeightRow1 maxWidthCol2 b12],
     hsep 4 center1 [alc maxHeightRow2 maxWidthCol1 b21,
                     alc maxHeightRow2 maxWidthCol2 b22]]
  where maxHeightRow1 = max (rows b11) (rows b12)
        maxHeightRow2 = max (rows b21) (rows b22)
        maxWidthCol1 = max (cols b11) (cols b21)
        maxWidthCol2 = max (cols b12) (cols b22)
        alc = align center1 center1

-- |Show four values in a grid
showGrid :: (Show a, Show b, Show c, Show d) => a -> b -> c -> d -> String
showGrid x11 x12 x21 x22 = render $ grid
  (showBox x11) (showBox x12)
  (showBox x21) (showBox x22)

instance (Show a) => Show (SMatrix i j a) where
  show Zero = "0"
  show Identity = "I"
  show (Dense x) = "Dense:\n" ++ show x
  show (Block x11 x12 x21 x22) = "Block:\n\n" ++
    showGrid x11 x12 x21 x22

printSMatrix :: (Show a) => SMatrix i j a -> IO ()
printSMatrix = putStrLn . show

