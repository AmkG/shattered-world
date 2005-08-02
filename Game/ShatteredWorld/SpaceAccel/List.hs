
module Game.ShatteredWorld.SpaceAccel.List(
  SpaceAccel,
  newSpaceAccel,
  lookupRectSpaceAccel
  ) where

import Data.Word
import Game.ShatteredWorld.Located

newtype SpaceAccel a = S [a]

newSpaceAccel = S

lookupRectSpaceAccel (S s) f ((x1, y1), (x2, y2))
  | x2 < x1    = lookupRectSpaceAccel (S s) f ((x2, y1), (x1, y2))
  | y2 < y1    = lookupRectSpaceAccel (S s) f ((x1, y2), (x2, y1))
  | otherwise  = core s where
    core []       = []
    core (a:as)
      | f a       = case located a of
                      (x, y) -> if (x1 <= x) && (x <= x2) && (y1 <= y) && (y <= y2) then a:core as else core as
      | otherwise = core as

