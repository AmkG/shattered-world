
-- a space-acceleration structure based on
-- Boundary Interval Hierarchy
module Game.ShatteredWorld.SpaceAccel.BIH(
  SpaceAccel,
  newSpaceAccel,
  lookupRectSpaceAccel
  ) where

import Control.Monad
import Data.Array.IO
import Data.List
import Data.Word
import System.IO.Unsafe

import Game.ShatteredWorld.Located

data SpaceAccel a = E
                  | L [a]
                  | SX !Word16 !Word16 (SpaceAccel a) (SpaceAccel a)
                  | SY !Word16 !Word16 (SpaceAccel a) (SpaceAccel a)
                  deriving(Show, Read)

xc :: Located a => a -> Word16
xc = fst . located
yc :: Located a => a -> Word16
yc = snd . located

newSpaceAccel :: Located a => [a] -> SpaceAccel a
newSpaceAccel as = unsafePerformIO start where
  length_as = length as

  -- convert to an array first to allow easy random
  -- access.  The partition process is similar to
  -- a quicksort, so a "good" partition is vital.
  start = do aa <- newListArray (0, length_as - 1) as
             corex aa 0 length_as

  -- interrecursion, alternating between X and Y axes
  corex = core corey finy xc SX
  corey = core corex finx yc SY

  -- if core failed to partition cleanly, then it invokes
  -- fin on the opposite axis.  If fin itself fails to
  -- partition cleanly, then it just makes a leaf node.
  finx = core corey makeL xc SX
  finy = core corex makeL yc SY

  core core retry cc mk aa lo hi
    | lo == hi                   = return E
    | lo + 1 == hi               = return . L . (:[]) =<< readArray aa lo
    | otherwise                  = do -- choose a split point
                                      split <- choosesplit cc aa lo hi
                                      -- partition using the split point
                                      mid <- partition split cc aa lo hi
                                      if mid == lo then -- failed to partition!  Use
                                                        -- retry.  For corex and corey,
                                                        -- this is the opposite fin.
                                                        -- For fin, just make a leaf
                                                        -- node.
                                                        retry aa lo hi
                                                   else do -- determine limits of the data
                                                           himin <- return split
                                                           lomax <- getmax cc aa lo mid
                                                           -- create sub-structures
                                                           sublo <- core aa lo mid
                                                           subhi <- core aa mid hi
                                                           return $ mk lomax himin sublo subhi
  -- construct a leaf node from a sub-array
  makeL :: IOArray Int a -> Int -> Int -> IO (SpaceAccel a)
  makeL aa lo hi = return . L =<< make aa lo hi [] where
    make aa lo hi rv
      | lo == hi    = return rv
      | otherwise   = do let i = hi - 1
                         a <- readArray aa i
                         make aa lo i (a:rv)

  -- chooses a splitting point
  choosesplit :: (a -> Word16) -> IOArray Int a -> Int -> Int -> IO Word16
  choosesplit cc aa lo hi
    -- lo == hi and lo+1 == hi are handled by core.
    | lo + 2 == hi        = do a <- readArray aa lo
                               b <- readArray aa (lo+1)
                               let ax = cc a ; bx = cc b
                               if ax < bx then return bx else return ax
    | otherwise           = do let mid = (lo + ((hi - lo) `div` 2))
                               a <- readArray aa lo
                               b <- readArray aa mid
                               c <- readArray aa (hi - 1)
                               let ax = cc a ; bx = cc b ; cx = cc c
                               return $ (sort [ax,bx,cx]) !! 1

  -- partitions the array, based on the given split point.
  -- returns the point of splitting.
  partition split cc aa = loop where
    loop lo hi
      | lo == hi                = return lo
      | otherwise               = do a <- readArray aa lo
                                     let ax = cc a
                                     if ax < split then loop (lo + 1) hi
                                                   else do let i = hi - 1
                                                           b <- readArray aa i
                                                           -- swap
                                                           writeArray aa lo b
                                                           writeArray aa i  a
                                                           -- continue
                                                           loop lo i

  -- determines the maximum in the array.
  getmax cc aa lo hi = loop lo hi 0 where
    loop lo hi m
      | lo == hi   = return m
      | otherwise  = do a <- readArray aa lo
                        let ax = cc a
                        loop (lo+1) hi (if ax > m then ax else m)

lookupRectSpaceAccel :: Located a => SpaceAccel a -> (a -> Bool) -> ((Word16, Word16), (Word16, Word16)) -> [a]
lookupRectSpaceAccel s f ((x1, y1), (x2, y2)) = start s f x1 y1 x2 y2 where
  start s f x1 y1 x2 y2
    | x1 > x2    = start s f x2 y1 x1 y2
    | y1 > y2    = start s f x1 y2 x2 y1
    | otherwise  = core f x1 y1 x2 y2 s
  core f x1 y1 x2 y2 = loop where
    loop E                           = []
    loop (L as)                      = filter inbound $ filter f as
    loop (SX lomax himin los his)    = check x1 x2 lomax himin los his
    loop (SY lomax himin los his)    = check y1 y2 lomax himin los his

    inbound a = let (x, y) = located a
                 in x1 <= x && x <= x2 && y1 <= y && y <= y2

    check lolim hilim lomax himin los his =
      let loscan
            | lolim <= lomax  = loop los
            | otherwise       = []
          hiscan
            | hilim >= himin  = loop his
            | otherwise       = []
       in loscan ++ hiscan


