
module Game.ShatteredWorld.Describe(
  Describe(..)
  ) where

class Describe a where
  describe :: a -> String

