
module Game.ShatteredWorld.ODA.Immutable(
  -- Interface-level documentation available in interface file
  -- Game.ShatteredWorld.ODA
  ODB, newODB,

  ODA,
  runWriteODA,
  runReadODA,

  readObject,
  writeObject,
  newObject,
  deleteObject,

  worldFragments,
  fragmentObjects,
  objectInFragment,
  setObjectFragment
  ) where

import Prelude hiding(id, (.))

import Control.Arrow
import Control.Category
import Control.Parallel
import System.IO

-- Data structures in use
import Data.Word
import qualified Data.Heap as Hp

import Game.ShatteredWorld.Object
import Game.ShatteredWorld.ArrowParallel

-- handles the allocation point
data AllocPt = AllocPt {
                 allocPtHeap :: !(Hp.MinHeap Word32),
                 allocPtNum :: !Word32
               }
newAllocPt = AllocPt Hp.empty 0

data ODB = ODB {
             odbAllocPt :: !AllocPt
           }

newODB _ = return $ ODB newAllocPt

-- Za Arro
data ODA a b = Rd (ODB -> a -> b)
             | Wr (ODB -> a -> (ODB, b))

instance Category ODA where
  id = Rd $ const id
  Rd f . Rd g = Rd $ \odb -> f odb . g odb
  Rd f . Wr g = Wr $ \odb gi -> let (odb', go) = g odb gi
                                 in (odb', f odb' go)
  Wr f . Rd g = Wr $ \odb gi -> let go = g odb gi
                                 in f odb go
  Wr f . Wr g = Wr $ \odb -> uncurry f . g odb

instance Arrow ODA where
  arr = Rd . const

  first (Rd f) = Rd $ first . f
  first (Wr f) = Wr $ \odb (fi, thru) -> let (odb', fo) = f odb fi
                                          in (odb', (fo, thru))

instance ArrowChoice ODA where
  -- left :: r a b -> r (Either a d) (Either b d)
  left (Rd f) = Rd $ \odb -> let core (Left b)   = Left $ f odb b
                                 core (Right d)  = Right d
                              in core
  left (Wr f) = Wr $ \odb -> let core (Left b)   = let (odb', fo) = f odb b
                                                    in (odb', Left fo)
                                 core (Right d)  = (odb, Right d)
                              in core

instance ArrowParallel ODA where
  Rd f /***/ Rd g = Rd $ \odb -> f odb /***/ g odb
  -- We can parallelize this: the read arrow cannot
  -- affect the operation of the write arrow.
  Rd f /***/ Wr g = Wr $ \odb inp -> let (fo, (odb', go)) = (f odb /***/ g odb) inp
                                      in (odb', (fo, go))
  -- We can't parallelize Wr f /***/ Rd g, as
  -- the f arrow may mutate state that the g
  -- arrow may read.
  f /***/ g = f *** g

runWriteODA :: ODA () () -> ODB -> IO ODB
runWriteODA (Rd _) odb = return odb -- uhm.  Why eval?
runWriteODA (Wr f) odb = let (odb', _) = f odb ()
                          in return odb'

runReadODA :: ODA a b -> ODB -> a -> b
runReadODA (Rd f) = f -- and you're done!

readObject = undefined
writeObject = undefined

newObject :: ODA () ObjectId
newObject = Wr nobj where
  nobj odb () = case Hp.view heap of
                  Just (id, nheap) -> (odb {odbAllocPt = ap {allocPtHeap = nheap}}, ObjectId id)
                  Nothing          -> (odb {odbAllocPt = ap {allocPtNum = num + 1}}, ObjectId num)
    where ap = odbAllocPt odb
          heap = allocPtHeap ap
          num = allocPtNum ap
deleteObject :: ODA ObjectId ()
deleteObject = Wr dobj where
  dobj odb (ObjectId oid) = if oid == num - 1 then (odb {odbAllocPt = ap {allocPtNum = oid}}, ())
                                              else (odb {odbAllocPt = ap {allocPtHeap = Hp.insert oid heap}}, ())
    where ap = odbAllocPt odb
          heap = allocPtHeap ap
          num = allocPtNum ap

worldFragments = undefined
fragmentObjects = undefined
objectInFragment = undefined
setObjectFragment = undefined

