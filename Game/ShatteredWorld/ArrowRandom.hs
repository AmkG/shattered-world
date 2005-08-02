{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.ShatteredWorld.ArrowRandom(
  RandomArrow(..),
  randA,
  runRandom
  ) where

import System.Random

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer

import Game.ShatteredWorld.ArrowParallel

import Prelude hiding ((.), id)

-- RandomGen g
-- a :: * -> * -> *
data RandomArrow g a b c =
    Arr (a b c)
  | ArrRand (g -> (g, a b c))

instance Category a => Category (RandomArrow g a) where
  id = Arr id
  Arr f     . Arr g      = Arr (f . g)
  Arr f     . ArrRand g  = ArrRand $ \gen -> case g gen of (gen', g') -> (gen', f  . g')
  ArrRand f . Arr g      = ArrRand $ \gen -> case f gen of (gen', f') -> (gen', f' . g)
  ArrRand f . ArrRand g  = ArrRand $ \gen -> case g gen of (gen', g') -> case f gen' of (gen'', f') -> (gen'', f' . g')

instance Arrow a => Arrow (RandomArrow g a) where
  arr f = Arr (arr f)
  first (Arr f)     = Arr (first f)
  first (ArrRand f) = ArrRand $ \gen -> case f gen of (gen', f') -> (gen', first f')

instance (Arrow a, ArrowParallel a) => ArrowParallel (RandomArrow g a) where
  Arr f     /***/ Arr g     = Arr $ f /***/ g
  Arr f     /***/ ArrRand g = ArrRand $ \gen -> case g gen of (gen', g') -> (gen', f  /***/ g')
  ArrRand f /***/ Arr g     = ArrRand $ \gen -> case f gen of (gen', f') -> (gen', f' /***/ g)
  ArrRand f /***/ ArrRand g = ArrRand $ \gen -> case f gen of (gen', f') -> case g gen' of (gen'', g') -> (gen'', f' /***/ g')

randA :: (RandomGen gen, Random c, Arrow r) => RandomArrow gen r () c
randA = ArrRand $ \gen -> case random gen of (c, g') -> (g', arr $ const c)

runRandom :: RandomArrow gen r a b -> gen -> (gen, r a b)
runRandom (Arr r)     g = (g, r)
runRandom (ArrRand f) g = f g

