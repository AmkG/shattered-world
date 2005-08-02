
module Game.ShatteredWorld.World.Fragment(
  FragmentId(..),
  Fragment(..),
  makeFragmentNames
  ) where

import Data.Word
import qualified Data.Set as Set
import System.Random
import Control.Monad
import Control.Monad.State

import Game.ShatteredWorld.RandomWeighted

newtype FragmentId = FragmentId Word8

data Fragment = Fragment {
                  fragmentId :: FragmentId,
                  fragmentName :: String
                  --TODO
                }

fragmentNameRandomize :: RandomGen g => g -> String
fragmentNameRandomize = evalState core where
  core = do a <- rand ; b <- rand ; c <- rand ; d <- rand
            if a && b && c && d then randChoice fixNames
                                else do g <- get
                                        let (gx, g') = split g
                                        put g'
                                        gen <- randChoice allGens
                                        return $ gen gx

  rand :: (RandomGen g, Random a) => State g a
  rand = do g <- get
            let (a, g') = random g
            put g'
            return a
  randChoice l = do i' <- rand
                    let i :: Int ; i = i'
                        sel = abs i `mod` length l
                    return $ l !! sel

fixNames =
  [ "silmaria"
  , "orphidia"
  , "deremeo"
  , "ulflund"
  , "seinen"
  , "hussiaria"
  , "dramidia"
  , "lexene"
  , "driss"
  ]

genElf g = let (g1, g') = split g
               (g2, g3) = split g'
            in    genRW g1 rwPrefix
               ++ genRW g2 rwBody
               ++ genRW g3 rwSuffix
  where rwPrefix = newRW
          [ ("eo", 1)
          , ("tho", 1)
          , ("thero", 1)
          , ("suwi", 1)
          , ("du", 1)
          , ("cla", 1)
          , ("la", 1)
          , ("ama", 1)
          , ("eia", 1)
          , ("sila", 1)
          , ("sinwa", 1)
          ]
        rwBody = newRW
          [ ("wal", 2)
          , ("lyn", 1)
          , ("non", 3)
          , ("drim", 1)
          , ("nin", 1)
          , ("law", 1)
          , ("wyn", 5)
          , ("vin", 3)
          , ("twil", 2)
          , ("tin", 1)
          , ("thul", 4)
          ]
        rwSuffix = newRW
          [ ("fel", 1)
          , ("aith", 1)
          , ("sel", 1)
          , ("sha", 1)
          , ("dell", 1)
          , ("amon", 1)
          , ("dore", 1)
          , ("van", 1)
          , ("ered", 1)
          ]

genBrit g = let (g1, g') = split g
                (g2, g3) = split g'
             in    genRW g1 rwPrefix
                ++ genRW g2 rwBody
                ++ genRW g3 rwSuffix
  where rwPrefix = newRW
          [ ("", 25)
          , ("inver", 1)
          , ("lin", 2)
          , ("kin", 2)
          , ("nor", 4)
          , ("suf", 4)
          , ("ast", 4)
          , ("aber", 1)
          , ("strath", 1)
          , ("dun", 5)
          , ("bre", 2)
          , ("brad", 3)
          , ("dal", 2)
          ]
        rwBody = newRW
          [ ("lun", 1)
          , ("blen", 1)
          , ("hart", 1)
          , ("ford", 1)
          , ("foss", 1)
          , ("glen", 1)
          , ("keld", 1)
          , ("streat", 1)
          , ("thorp", 1)
          , ("whel", 1)
          , ("gilyng", 1)
          , ("thorn", 1)
          , ("wort", 1)
          , ("wilhem", 1)
          ]
        rwSuffix = newRW
          [ ("wick", 2)
          , ("port", 3)
          , ("mouth", 1)
          , ("chester", 4)
          , ("ham", 5)
          , ("field", 2)
          , ("dale", 2)
          , ("bury", 1)
          , ("combe", 3)
          , ("cott", 2)
          , ("ney", 3)
          ]
genJap g = let (g1, g') = split g
               (g2, g3) = split g'
            in genRW g1 rwPrefix (genRW g2 rwBody ++ genRW g3 rwSuffix)
  where -- the prefix 'o-' must be treated specially:
        -- it converts "k" to "g", "s" to "z", "t" to "d"
        -- and "sh" to "j" when used.
        rwPrefix = newRW
          [ (id, 20)
          , (("shin"++), 5)
          , (("u"++), 2)
          , (("sa"++), 2)
          , (("kita"++), 2)
          , (makeO, 4)
          ] where makeO ('k':cs)         = "og" ++ cs
                  makeO ('t':cs)         = "od" ++ cs
                  makeO ('s':'h':cs)     = "oj" ++ cs
                  makeO ('s':cs)         = "oz" ++ cs
                  makeO cs               = "o" ++ cs
        rwBody = newRW
          [ ("karasu", 2)
          , ("tera", 2)
          , ("kamo", 2)
          , ("sei", 2)
          , ("heian", 2)
          , ("matsu", 2)
          , ("fuku", 2)
          , ("naka", 2)
          , ("ichi", 1)
          , ("san", 1)
          , ("go", 1)
          , ("fushimi", 2)
          , ("hon", 1)
          ]
        rwSuffix = newRW
          [ ("machi", 2)
          , ("jima", 2)
          , ("cho", 4)
          , ("mura", 4)
          , ("shi", 5)
          , ("gun", 5)
          , ("gawa", 4)
          , ("zumi", 2)
          , ("ko", 3)
          , ("jo", 3)
          , ("johoku", 1)
          , ("josai", 1)
          , ("jonan", 1)
          , ("gami", 2)
          ]

genGerm g = let (g'', g') = split g
                (g1, g2) = split g'
                (g3, g4) = split g''
             in    genRW g1 rwPrefix
                ++ genRW g2 rwBody
                ++ genRW g3 rwSuffix
                ++ genRW g4 rwAttach
  where rwPrefix = newRW
          [ ("", 25)
          , ("alt", 4)
          , ("grossen", 2)
          , ("ober", 3)
          , ("klein", 3)
          , ("nien", 2)
          , ("nieder", 2)
          , ("grun", 2)
          ]
        rwBody = newRW
          [ ("inn", 1)
          , ("saltz", 1)
          , ("pass", 1)
          , ("regen", 1)
          , ("wernige", 1)
          , ("mann", 1)
          , ("hans", 1)
          , ("wuster", 1)
          ]
        rwSuffix = newRW
          [ ("gau", 1)
          , ("werth", 1)
          , ("bach", 1)
          , ("bruck", 1)
          , ("burg", 3)
          , ("hausen", 3)
          , ("dorf", 2)
          , ("felde", 2)
          , ("heim", 2)
          ]
        rwAttach = newRW
          [ ("", 25)
          , ("-hof", 1)
          , ("-siedlung", 1)
          , ("-ausbau", 1)
          ]

allGens :: RandomGen g => [g -> String]
allGens = [ genElf
          , genBrit
          , genJap
          , genGerm
          ]

-- weirdest markov-generated name ever: wylensefarlouderthencar

makeFragmentNames :: (Num int, RandomGen g) => int -> g -> [String]
makeFragmentNames = core Set.empty where
  core done 0 _  = Set.toList done
  core done i g  = let (g', gx) = split g
                       name = fragmentNameRandomize gx
                    in if Set.member name done then core done i g'
                                               else core (Set.insert name done) (i - 1) g'

testFragmentNameRandomize :: IO ()
testFragmentNameRandomize = do
  forM_ [1..10] $ \_ -> do
    g <- getStdGen
    let (g', gx) = split g
    setStdGen g'
    putStrLn $ fragmentNameRandomize gx
