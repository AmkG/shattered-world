
module Game.ShatteredWorld.Located(Located(..)) where

import Data.Word

class Located a where
  located :: a -> (Word16, Word16)

-- for debugging
instance (Integral a, Integral b) => Located ((,) a b) where
  located (a, b) = (fromIntegral a, fromIntegral b)

