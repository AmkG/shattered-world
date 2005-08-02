
-- Data type that allows searching for items located
-- within a rectangle.
module Game.ShatteredWorld.SpaceAccel(
  SpaceAccel,
  -- newSpaceAccel :: Located a => [a] -> SpaceAccel a
  newSpaceAccel,
  -- searches for points inside the given rectangle
  -- lookupRectSpaceAccel :: Located a => SpaceAccel a -> (a -> Bool) -> ((Word16, Word16), (Word16, Word16)) -> [a]
  lookupRectSpaceAccel
  ) where

import Game.ShatteredWorld.SpaceAccel.List

