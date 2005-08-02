
{-
Selects randomly according to weight
-}

module Game.ShatteredWorld.RandomWeighted(
  RW, newRW, genRW
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), index)
import Data.List
import System.Random
import Control.Monad

data RW a = RW Int (Seq (a, Int)) deriving (Show, Read)

newRW :: [(a, Int)] -> RW a
newRW = foldl' combine (RW 0 Seq.empty) where
  combine (RW i s) (a, i2) = RW (i + i2) (s |> (a, i))

genRW :: RandomGen g => g -> RW a -> a
genRW g (RW i s) = let (i_raw, _) = random g
                       i_sel = abs i_raw `mod` i
                    in seek i_sel s 0 (Seq.length s - 1)

seek i_sel s = loop where
  loop lo hi
    | lo == hi    = let (a, i) = index s lo
                     in if i <= i_sel then a
                                      else let (a2, _) = index s (lo - 1)
                                            in a2
    | otherwise   = let mid = lo + ((hi - lo) `div` 2)
                        (_, i) = index s mid
                     in if i < i_sel then loop (mid + 1) hi
                                     else loop lo mid

testRW :: Show a => [(a, Int)] -> IO ()
testRW inp = do let rw = newRW inp
                forM_ [1..20] $ \_ -> do
                  g <- getStdGen
                  let (g', gx) = split g
                  setStdGen g'
                  putStrLn $ show (genRW gx rw)
                return ()

