
{-
ODA is the Object Database Arrow.

ODA's are used to model transformations of one
ODB (Object DataBase), by one concurrent thread.
while allowing another concurrent thread to
read the same ODB.

This module is a wrapper.  The Game/ShatteredWorld/ODA/
directory contains the implementations of ODA.  Only
one is used for each compile, but we have multiple
implementations in order to allow us to experiment
with various ways of storing and manipulating ODB's.
-}

module Game.ShatteredWorld.ODA(
  -- The ODB type is the actual object database.
  -- It is largely abstract at this level.  The
  -- implementation provides this for us.
  ODB,
  -- newODB :: WorldInfo -> IO ODB
  -- Creates a new object database, based on
  -- a prebuilt world.
  newODB,

  -- The ODA arrow type handles the transition
  -- of data inside an ODB.
  ODA,
  -- ODA should have instances of:
  --   Category ODA
  --   Arrow ODA
  --   ArrowChoice ODA
  --   ArrowApply ODA
  --   ArrowParallel ODA (from Game.ShatteredWorld.ArrowParallel)

  -- runWriteODA :: ODA () () -> ODB -> IO ODB
  -- Executes an ODA arrow, and handle all writes
  -- to the database inside the arrow.  The caller
  -- promises that only one IO thread will ever
  -- execute runWriteODA.  The implementation may
  -- do various tricks to save space or prevent GC,
  -- so the only writes to a database are allowed
  -- inside a runWriteODA.
  -- The implementation may cause older
  -- versions created at random points to be
  -- invalid or corrupt; that is, the ODB should
  -- be linear, except perhaps the most recent.
  --
  -- The expected use is that the UI renders the
  -- current ODB while the simulation thread
  -- computes the next ODB, with both UI and sim
  -- reading the ODB, but only the sim thread
  -- writing a "new" one.  Once a new ODB has
  -- been produced, both threads synchronize
  -- and drop references to the old ODB.
  --
  runWriteODA,
  -- runReadODA :: ODA a b -> ODB -> a -> b
  -- Executes an ODA arrow, with the promise that
  -- no writes to the database will be performed.
  -- The implementation may use a common space for
  -- writes, so runReadODB cannot mutate the
  -- database, not even transiently - even though
  -- the type here shows that a new ODB would not
  -- be returned anyway.
  -- The implementation promises that multiple
  -- parallel runReadODA calls will be safe.
  runReadODA,

  -- Object query (read-only)
  -- readObject :: ODA ObjectId Object
  readObject,

  -- Object Write operations
  -- writeObject :: ODA Object ()
  writeObject,
  -- newObject :: ODA () ObjectId
  -- newObject allocates the lowest free object ID.
  -- we explictly specify it being the lowest to
  -- enforce determinism of allocated ID's.
  newObject,
  -- deleteObject :: ODA ObjectId ()
  deleteObject,
  -- notice that the above means that we must
  -- manage objects manually!

  -- Fragment query (read-only)
  -- worldFragments :: ODA () Word8
  -- returns the number of world fragments.  You
  -- can then generate the actual world fragments
  -- using the FragmentId constructor.
  worldFragments,
  -- fragmentObjects :: ODA FragmentId [ObjectId]
  fragmentObjects,
  -- objectInFragment :: ODA (ObjectId, FragmentId) Bool
  objectInFragment,

  -- Fragment write operations
  -- objects are initially created outside of any fragment;
  -- you should setObjectFragment a newly-created object
  -- setObjectFragment :: ODA (ObjectId, FragmentId) ()
  setObjectFragment,


  -- defined here
  mapReduceFragments

  ) where

-- implementation using double buffered mutable unpacked parallel arrays.
-- import Game.ShatteredWorld.ODA.DoubleMU

-- implementation using idiomatic immutable datatypes.
import Game.ShatteredWorld.ODA.Immutable


-- used here
import Game.ShatteredWorld.ArrowParallel
import Game.ShatteredWorld.World.Fragment

import Control.Arrow

mapReduceFragments :: ODA FragmentId a -> ODA (a, a) a -> ODA () a
mapReduceFragments mr rr =     worldFragments
                           >>> arr (\i -> (0, i - 1))
                           >>> loop where
  loop =     arr choose
         >>> enmap ||| (loop /***/ loop >>> rr)
  choose (lo, hi)
    | lo == hi   = Left lo
    | otherwise  = let mid = ((hi - lo) `div` 2) + lo
                    in Right ((lo, mid), (mid + 1, hi))
  enmap = arr FragmentId >>> mr
