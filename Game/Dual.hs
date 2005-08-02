{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- | Dual is a simple, two-threaded game engine, where a Display
     thread handles user interface concerns and sends messages to
     a Simulation thread.  The Simulation thread then processes
     messages and updates the current game state, which may be
     accessed by the Display thread.

     The type of messages and game states is up to the user.

     As a general example, if you want a physics simulation with
     (for good numeric behavior) constant time step, you would
     want to have a message that means "compute the next time
     step".  Your Display thread executes 'getGameState' to sample
     the current game state, then sends the "compute next time
     step" message, and goes on rendering the game state sampled.
     Your Display thread then waits around for the game state to
     advance, then waits around for the next time step, and so on.

     Your Simulation thread just receives messages, possibly
     caching user input for the next cycle, then computes a new
     game state when it receives the "compute next time step"
     message, using 'Control.Parallel' for better
     parallelization.
-}

module Game.Dual(
  DispIO,
  SimState,
  dual,
  sendGameMessage,
  receiveGameMessage,
  getGameState,
  forkDispIO
  ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Data.IORef

{- | DispIO is the monad for the Display thread.
     It is a 'MonadIO', so you can perform IO
     operations (such as writing to the screen)
     from it by 'liftIO'.  Display threads can
     also send a message to the simulation thread
     by 'sendGameMessage', and can snoop the
     current game state by 'getGameState'.
-}
newtype DispIO msg gs a = D (StateT (Chan msg, IORef gs) IO a)

instance Monad (DispIO msg gs) where
  D f >>= g = D (f >>= \a -> case g a of
                               D g' -> g')
  return a = D (return a)
  fail e = D (fail e)

instance MonadIO (DispIO msg gs) where
  liftIO mio = D (liftIO mio)

{- | SimState is the monad for the Simulation thread.
     It is a 'MonadState' with the state being the given
     gamestate gs.
-}
newtype SimState msg gs a = S (StateT (Chan msg, IORef gs) IO a)

instance Monad (SimState msg gs) where
  S f >>= g = S (f >>= \a -> case g a of
                               S g' -> g')
  return a = S (return a)
  fail e = S (fail e)

instance MonadState gs (SimState msg gs) where
  get = S (do (_, gsv) <- get
              liftIO $ readIORef gsv)
  put gs = S (do (_, gsv) <- get
                 liftIO $ writeIORef gsv gs)

{- | Executes a Display and Simulation thread, and
     waits for the Display thread to end.
-}
dual :: (DispIO msg gs a) -> (SimState msg gs b) -> gs -> IO a
dual (D dt) (S st) ini_gs = do gsv <- newIORef ini_gs
                               msv <- newChan
                               forkIO $ do evalStateT st (msv, gsv)
                                           return ()
                               evalStateT dt (msv, gsv)

{- | sends a game message from the Display thread to
     the Simulation thread.
-}
sendGameMessage :: msg -> DispIO msg gs ()
sendGameMessage m = D $ do (msv, _) <- get
                           liftIO $ writeChan msv m

{- | receives a message in the Simulation thread. -}
receiveGameMessage :: SimState msg gs msg
receiveGameMessage = S $ do (msv, _) <- get
                            liftIO $ readChan msv

{- | inspect the game state in the Display thread -}
getGameState :: DispIO msg gs gs
getGameState = D $ do (_, gsv) <- get
                      liftIO $ readIORef gsv

{- | creates a new Display thread. -}
forkDispIO :: DispIO msg gs () -> DispIO msg gs ThreadId
forkDispIO (D f) = D $ do state <- get
                          liftIO $ forkIO $ evalStateT f state

