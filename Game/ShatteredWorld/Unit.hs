
module Game.ShatteredWorld.Unit(
  UnitClass(..), Ability(..),
  UnitType(..), unitTypeRandomize
  ) where

import Game.ShatteredWorld.Unit.Class
import Game.ShatteredWorld.Unit.Ability

import System.Random
import Control.Monad.State

-- TODO: other fields, better accessors
data UnitType = UT String UnitClass [Ability] !Int 
              deriving(Show, Read, Eq)

unitTypeRandomize :: RandomGen g => UnitClass -> (String -> Bool) -> g -> (Int -> UnitType)
unitTypeRandomize uc nameChecker = fst . runState make where
  make = do g <- get
            -- generate abilities
            let (g1, g2) = split g
                abilities = abilityRandomize uc g2
            put g1
            s <- name abilities
            return $ UT s uc abilities
  name abilities = -- generate name.  If nameChecker returns True, generate
                   -- a new one.
                   -- Note: for various reasons, there cannot be more than 256
                   -- unit types (just think of the administrative overhead on
                   -- the player!).  There are 16 suffix candidates for each
                   -- unit class, and there are likely to be more than 16 prefix
                   -- candidates.  Thus, there are more than 256 potential
                   -- names for a new unit type, making it unlikely that all
                   -- of them are already taken in a game.
                   let allPrefixCandidates = concatMap abilityPrefixCandidates abilities ++ prefixCandidates
                       allSuffixCandidates = unitClassSuffixCandidates uc
                       loop = do prefix <- randSel allPrefixCandidates
                                 suffix <- randSel allSuffixCandidates
                                 let fullname = prefix ++ " " ++ suffix
                                 if (nameChecker fullname) then loop
                                                           else return fullname
                    in loop

  randSel [x]  = return x
  randSel xs   = do c <- rand
                    if c then randSel $ evens xs
                         else randSel $ odds xs

  rand = do g <- get
            let (rv, g2) = random g
            put g2
            return rv

  -- gets only even indexed items
  evens []       = []
  evens [x]      = [x]
  evens (x:_:xs) = x:evens xs
  -- gets only odd indexed items
  odds []        = []
  odds (_:xs)    = evens xs

  -- additional candidates for prefix
  prefixCandidates :: [String]
  prefixCandidates = [ "Silmarian", "Orphidian", "Deremov", "Ulflandish", "Seinenese", "Hussiar", "Dramidic", "Lexite", "Driss"
                     , "Lion", "Griffin", "Malkin", "Unicorn"
                     ]

-- debugging/sampling
tryUnitTypeRandomize :: UnitClass -> [Int] -> IO ()
tryUnitTypeRandomize uc = mapM_ mkOne where
  mkOne s = let g = mkStdGen s
             in do putStr "  "
                   putStr $ show (unitTypeRandomize uc (const False) g s)
                   putStrLn ""

