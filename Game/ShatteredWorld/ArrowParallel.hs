{-
Module that provides a simple instance for
parallel execution of arrows.
-}

module Game.ShatteredWorld.ArrowParallel(ArrowParallel(..)) where

import Control.Arrow
import Control.Parallel

infixr 3 /***/
infixr 3 /&&&/

class Arrow r => ArrowParallel r where
  -- analogous to ***, but attempt to spark the
  -- computations if possible.
  -- NOTE!  Arrow *** actually requires that the
  -- first input arrow semantically gets executed
  -- before the second.  Thus, it is possible to
  -- parallelize only if the first input arrow
  -- cannot possibly affect the operation of the
  -- second arrow.  The ordering caused by Arrow
  -- *** should be preserved by /***/; /***/
  -- should only parallelize if it will not
  -- be observable.
  (/***/) :: r a b -> r c d -> r (a, c) (b, d)

  -- analogous to &&&, but attempt to spark the
  -- computations if possible
  (/&&&/) :: r a b -> r a d -> r a (b, d)
  f /&&&/ g = arr dup >>> (f /***/ g) where
    dup a = (a,a)

-- base instances
instance ArrowParallel (->) where
  (/***/) f g ~(x, y) = let fx = f x
                            gy = g y
                         in fx `par` gy `pseq` (fx, gy)

