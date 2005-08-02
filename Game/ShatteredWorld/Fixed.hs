
-- simple fixed point library
module Game.ShatteredWorld.Fixed(Fixed) where

import Data.Int
import Data.Bits
import Data.Ratio

point :: Int
point = 24
expt :: Int32
expt = 1 `shiftL` point
mask :: Int32
mask = expt - 1

newtype Fixed = F Int32 deriving(Eq, Ord)

instance Show Fixed where
  show f = show $ fromRational $ toRational f

instance Num Fixed where
  F i + F j = F $ i + j
  F i - F j = F $ i - j
  F i * F j = let i' :: Int64
                  i' = fromIntegral i
                  j' :: Int64
                  j' = fromIntegral j
                  prod = (i' * j') `shiftR` point
               in F $ fromIntegral prod
  negate (F i) = F $ negate i
  abs (F i) = F $ abs i
  signum (F i) = F $ signum i * expt
  fromInteger i = F $ fromInteger $ i `shiftL` point

instance Real Fixed where
  toRational (F i) = fromIntegral i % fromIntegral expt

instance Fractional Fixed where
  F i / F j = let i' :: Int64
                  i' = fromIntegral i `shiftL` point
                  j' :: Int64
                  j' = fromIntegral j
                  quot = i' `div` j'
               in F $ fromIntegral quot
  fromRational r = let i' :: Int64
                       i' = (fromIntegral $ numerator r) `shiftL` point
                       j' :: Int64
                       j' = fromIntegral $ denominator r
                       quot = i' `div` j'
                    in F $ fromIntegral quot

instance RealFrac Fixed where
  properFraction f@(F i)
    | i < 0     = case properFraction (F $ negate i) of
                    (b, a) -> (negate b, negate a)
    | i < expt  = (0, f)
    | otherwise = let whole = fromIntegral (i `shiftR` point)
                      frac = i .&. mask
                   in (whole, F frac)


