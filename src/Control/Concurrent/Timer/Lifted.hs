{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Timer.Lifted
( Timer
, TimerIO

, oneShotTimer
, oneShotRestart

, repeatedTimer
, repeatedRestart

, newTimer
, stopTimer
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Lifted         (ThreadId, fork, killThread)
import           Control.Concurrent.MVar.Lifted    (newMVar, tryTakeMVar, putMVar, modifyMVar_)
import           Control.Concurrent.Suspend.Lifted (Delay, suspend)
import           Control.Monad
import           Control.Monad.Base                (MonadBase)
import           Control.Monad.Trans.Control       (MonadBaseControl)
------------------------------------------------------------------------------
import           Control.Concurrent.Timer.Types (Timer(..))
------------------------------------------------------------------------------

-- | Attempts to restart (or start) a timer making it a one shot timer.
--
-- Returns True if the restrat was successful,
-- otherwise (e.g. other thread is attempting to restart the timer) returns False.
oneShotRestart :: MonadBaseControl IO m
               => Timer m
               -> m Bool
oneShotRestart (Timer action delay threadID) = do
    mtid <- tryTakeMVar threadID
    case mtid of
         Just (Just tid) -> do
             killThread tid
             ntid <- Just <$> oneShotAction delay action
             putMVar threadID ntid
             return True
         Just (Nothing)  -> do
             ntid <- Just <$> oneShotAction delay action
             putMVar threadID ntid
             return True
         Nothing -> return False
{-# INLINEABLE oneShotRestart #-}

-- | Attempts to restart (or start) a timer making it a repeated timer.
--
-- Returns True if the restrat was successful,
-- otherwise (e.g. other thread is attempting to restart the timer) returns False.
repeatedRestart :: MonadBaseControl IO m
                => Timer m
                -> m Bool
repeatedRestart (Timer action delay threadID) = do
    mtid <- tryTakeMVar threadID
    case mtid of
         Just (Just tid) -> do
             killThread tid
             ntid <- Just <$> repeatedAction delay action
             putMVar threadID ntid
             return True
         Just (Nothing)  -> do
             ntid <- Just <$> repeatedAction delay action
             putMVar threadID ntid
             return True
         Nothing -> return False
{-# INLINEABLE repeatedRestart #-}

-- | Executes the the given action once after the given delay elapsed, no sooner, maybe later.
oneShotTimer :: MonadBaseControl IO m
              => Delay      -- ^ The (minimal) time until the execution in microseconds.
             -> m ()        -- ^ The action to be executed.
             -> m (Timer m)
oneShotTimer d action = do
    tid <- oneShotAction d action >>= newMVar . Just
    return Timer { timerAction   = action
                 , timerDelay    = d
                 , timerThreadID = tid
                 }
{-# INLINEABLE oneShotTimer #-}

-- | Executes the the given action repeatedly with at least the given delay between executions.
repeatedTimer :: MonadBaseControl IO m
              => Delay   -- ^ The (minimal) delay between executions.
              -> m ()    -- ^ The action to be executed.
              -> m (Timer m)
repeatedTimer d action = do
    tid <- repeatedAction d action >>= newMVar . Just
    return Timer { timerAction   = action
                 , timerDelay    = d
                 , timerThreadID = tid
                 }
{-# INLINEABLE repeatedTimer #-}

-- | This function is blocking. It waits until it can stop the timer
-- (until there is a value in the threadID MVar), then it kill the thread.
stopTimer :: MonadBaseControl IO m
          => Timer m
          -> m ()
stopTimer (Timer _ _ threadID) = modifyMVar_ threadID $
    maybe (return Nothing)
          (\tid -> killThread tid >> return Nothing)

-- | Creates a new timer. This does not start the timer.
newTimer :: MonadBase IO m
         => Delay   -- ^ The (minimal) delay between executions.
         -> m ()    -- ^ The action to be executed.
         -> m (Timer m)
newTimer d action = Timer action d <$> newMVar Nothing
{-# INLINE newTimer #-}

------------------------------------------------------------------------------
-- | Utility

type TimerIO = Timer IO

-- | Forks a new thread that runs the supplied action
-- (at least) after the given delay.
oneShotAction :: MonadBaseControl IO m
              => Delay
              -> m ()
              -> m ThreadId
oneShotAction d action = fork (suspend d >> action)
{-# INLINE oneShotAction #-}

-- | Forks a new thread that repeats the supplied action
-- with (at least) the given delay between each execution.
repeatedAction :: MonadBaseControl IO m
               => Delay
               -> m ()
               -> m ThreadId
repeatedAction d action = fork (forever $ suspend d >> action)
{-# INLINE repeatedAction #-}
