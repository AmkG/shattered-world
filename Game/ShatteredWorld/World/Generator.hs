
module Game.ShatteredWorld.World.Generator(
  -- TODO
  ) where

import System.Random
import Data.List
import Control.Monad.State

import Debug.Trace

worldwide = 256 :: Int
worldheight = 192 :: Int

-- creates a square grid, where the length of a side is given.
-- We distribute by dividing the sides by 256 (192 for
-- vertical) using digital direct synthesis.
squareGrid sides = [(x, y) | x <- divide sides worldwide, y <- divide sides worldheight]

-- performs digital direct synthesis
divide n d = recur ((n - d) `div` 2) 0 where
  -- we increment count by numerator, then when we have reached
  -- positivity, we decrement by d - n (actually equal to adding
  -- n then subtracting d also, but we can precompute d - n and
  -- reduce to a single adder each time).  You'll have to consult a
  -- numerical analyst to find out how it works - I just memorized
  -- the algo and have no real idea why it works.  Apparently
  -- numbers above 0 represent phases on a circle that are beyond
  -- 180 degrees and each update represents an adjustment of the
  -- phase, or something.
  diff = d - n
  recur count coord
    | coord == d  = []
    | count < 0   = recur (count + n) (coord + 1)
    | otherwise   = coord:recur (count - diff) (coord + 1)

square x = x * x

evens :: Integral i => [i]
evens = map (2 *) [1..]
evenSquares :: Integral i => [i]
evenSquares = map square evens

odds :: Integral i => [i]
odds = map (\x -> x - 1) evens
oddSquares :: Integral i => [i]
oddSquares = map square odds

largestEvenSquare :: Integral i => i -> i
largestEvenSquare n = let pairs = zip evens (tail evenSquares)
                       in maybe 0 fst $ (findLargestSnd n) pairs
largestOddSquare :: Integral i => i -> i
largestOddSquare n = let pairs = zip odds (tail oddSquares)
                       in maybe 1 fst $ (findLargestSnd n) pairs
findLargestSnd n = find (\ (_, x) -> x > n )

-- Given a number of fragments, generate a set of coordinates
-- world coordinates are in a 256x192 grid.
coordRandomize :: RandomGen g => Int -> g -> [(Int, Int)]
coordRandomize nfrags = fst . runState make where
  -- Algorithm: To ensure reasonable distribution, we set
  -- up initial square grids.  A square grid is simply a
  -- flatly-distributed grid of coordinates that are some
  -- numeric square.
  -- From the square grids we randomly perturb the location of
  -- fragments.

  -- note that we try to avoid odd square grids because
  -- all odd square grids will overlap at (128, 96).  So
  -- only use an odd square grid once if the number of
  -- fragments is odd, and use only even square grids
  -- afterward.
  make
    | nfrags < 0   = undefined -- impossible!
    | odd nfrags   = let oddsqr = largestOddSquare nfrags
                         next = nfrags - square oddsqr
                      in do sq1 <- gridSquareRandomize oddsqr []
                            sq2 <- core next []
                            fixup $ sq1 ++ sq2
    | otherwise    = core nfrags [] >>= fixup

  -- tries to get as many fragments into a square as possible.
  -- no more fragments to get
  core 0 acc = return acc
  -- 2 is not a square, and the next even square is 4.
  -- TODO: just throw in two fragments randomly
  core 2 acc = return acc
  -- get as many remaining fragments as we can
  core n acc = let evensqr = largestEvenSquare n
                   next = n - square evensqr
                in gridSquareRandomize evensqr acc >>= core next

  gridSquareRandomize side acc = let inigrid = squareGrid side
                                     variance = (worldwide `div` 2) `div` side
                                  in do rv <- randomize variance inigrid
                                        return $ rv ++ acc

  randomize variance = mapM (randomBy variance) where
    randomBy variance (x, y) = do xr <- rand ; xc <- rand
                                  yr <- rand ; yc <- rand
                                  let xv = xr `mod` (if xc then (-variance) else variance)
                                      yv = yr `mod` (if yc then (-variance) else variance)
                                      x' = x + xv
                                      y' = y + yv
                                      x'' = truncateRange 0 x' (worldwide - 1)
                                      y'' = truncateRange 0 y' (worldheight - 1)
                                  return (x'', y'')
      where
        -- core random number generator
        rand :: (Random a, RandomGen g) => State g a
        rand = do g1 <- get
                  let (rv, g2) = random g1
                  put g2
                  return rv

  fixup = return . id -- TODO: avoid overlap

  truncateRange lo mid hi
    | lo > mid  = lo
    | mid > hi  = hi
    | otherwise = mid

