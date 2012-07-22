{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Timer.Lifted
( Timer
, TimerIO

, oneShotTimer
, oneShotStart
, oneShotRestart

, repeatedTimer
, repeatedStart
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
import           Control.Concurrent.Timer.Types (Timer(..), TimerImmutable(..))
------------------------------------------------------------------------------

-- | Attempts to start a timer.
-- The started timer will have the given delay and action associated and will be one-shot timer.
--
-- If the timer was already initialized, it the previous timer will be stoped (the thread killed) and the timer will be started anew.
--
-- Returns True if the strat was successful,
-- otherwise (e.g. other thread is attempting to manipulate the timer) returns False.
oneShotStart :: MonadBaseControl IO m
               => Timer m
               -> m ()  -- ^ The action the timer will start with.
               -> Delay -- ^ The dealy the timer will start with.
               -> m Bool
oneShotStart (Timer mvmtim) a d = do
    mtim <- tryTakeMVar mvmtim
    case mtim of
         Just (Just (TimerImmutable _ _ tid)) -> do
             killThread tid
             oneShotTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         Just (Nothing) -> do
             oneShotTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         Nothing -> return False
{-# INLINEABLE oneShotStart #-}

-- | Attempts to start a timer.
-- The started timer will have the given delay and action associated and will be repeated timer.
--
-- If the timer was already initialized, it the previous timer will be stoped (the thread killed) and the timer will be started anew.
--
-- Returns True if the strat was successful,
-- otherwise (e.g. other thread is attempting to manipulate the timer) returns False.
repeatedStart :: MonadBaseControl IO m
                => Timer m
                -> m ()   -- ^ The action the timer will start with.
                -> Delay  -- ^ The dealy the timer will start with.
                -> m Bool
repeatedStart (Timer mvmtim) a d = do
    mtim <- tryTakeMVar mvmtim
    case mtim of
         Just (Just (TimerImmutable _ _ tid)) -> do
             killThread tid
             repeatedTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         Just (Nothing) -> do
             repeatedTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         Nothing -> return False
{-# INLINEABLE repeatedStart #-}

-- | Attempts to restart already initialized timer.
-- The restarted timer will have the same delay and action associated and will be one-shot timer.
--
-- Returns True if the restrat was successful,
-- otherwise (e.g. other thread is attempting to manipulate the timer or the timer was not initialized) returns False.
oneShotRestart :: MonadBaseControl IO m
               => Timer m
               -> m Bool
oneShotRestart (Timer mvmtim) = do
    mtim <- tryTakeMVar mvmtim
    case mtim of
         Just (Just (TimerImmutable a d tid)) -> do
             killThread tid
             oneShotTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         _ -> return False
{-# INLINEABLE oneShotRestart #-}

-- | Attempts to restart already initialized timer.
-- The restarted timer will have the same delay and action associated and will be one-shot timer.
-- 
-- Returns True if the restrat was successful,
-- otherwise (e.g. other thread is attempting to manipulate the timer or the timer was not initialized) returns False.
repeatedRestart :: MonadBaseControl IO m
                => Timer m
                -> m Bool
repeatedRestart (Timer mvmtim) = do
    mtim <- tryTakeMVar mvmtim
    case mtim of
         Just (Just (TimerImmutable a d tid)) -> do
             killThread tid
             repeatedTimerImmutable a d >>= putMVar mvmtim . Just
             return True
         _ -> return False
{-# INLINEABLE repeatedRestart #-}

-- | Executes the the given action once after the given delay elapsed, no sooner, maybe later.
oneShotTimer :: MonadBaseControl IO m
             => m ()        -- ^ The action to be executed.
             -> Delay      -- ^ The (minimal) time until the execution in microseconds.
             -> m (Timer m)
oneShotTimer a d = Timer <$> (oneShotTimerImmutable a d >>= newMVar . Just)
{-# INLINE oneShotTimer #-}

-- | Executes the the given action repeatedly with at least the given delay between executions.
repeatedTimer :: MonadBaseControl IO m
              => m ()    -- ^ The action to be executed.
              -> Delay   -- ^ The (minimal) delay between executions.
              -> m (Timer m)
repeatedTimer a d = Timer <$> (repeatedTimerImmutable a d >>= newMVar . Just)
{-# INLINE repeatedTimer #-}

-- | This function is blocking. It waits until it can stop the timer
-- (until there is a value in the MVar), then it kills the timer's thread.
--
-- After this action completes, the Timer is not innitialized anymore (the MVar contains Nothing).
stopTimer :: MonadBaseControl IO m
          => Timer m
          -> m ()
stopTimer (Timer mvmtim) = modifyMVar_ mvmtim $
    maybe (return Nothing)
          (\(TimerImmutable _ _ tid) -> killThread tid >> return Nothing)
{-# INLINE stopTimer #-}

-- | Creates a new timer. This does not start the timer.
newTimer :: MonadBase IO m
         => m (Timer m)
newTimer = Timer <$> newMVar Nothing
{-# INLINE newTimer #-}

------------------------------------------------------------------------------
-- | Utility

type TimerIO = Timer IO

-- | Forks a new thread that runs the supplied action
-- (at least) after the given delay and stores the action,
-- delay and thread id in the immutable TimerImmutable value.
oneShotTimerImmutable :: MonadBaseControl IO m
                      => m ()        -- ^ The action to be executed.
                      -> Delay       -- ^ The (minimal) time until the execution in microseconds.
                      -> m (TimerImmutable m)
oneShotTimerImmutable a d = TimerImmutable a d <$> oneShotAction a d
{-# INLINE oneShotTimerImmutable #-}

-- | Forks a new thread that repeats the supplied action
-- with (at least) the given delay between each execution and stores the action,
-- delay and thread id in the immutable TimerImmutable value.
repeatedTimerImmutable :: MonadBaseControl IO m
                       => m ()        -- ^ The action to be executed.
                       -> Delay       -- ^ The (minimal) time until the execution in microseconds.
                       -> m (TimerImmutable m)
repeatedTimerImmutable a d = TimerImmutable a d <$> repeatedAction a d
{-# INLINE repeatedTimerImmutable #-}

-- | Forks a new thread that runs the supplied action
-- (at least) after the given delay.
oneShotAction :: MonadBaseControl IO m
              => m ()
              -> Delay
              -> m ThreadId
oneShotAction action delay = fork (suspend delay >> action)
{-# INLINE oneShotAction #-}

-- | Forks a new thread that repeats the supplied action
-- with (at least) the given delay between each execution.
repeatedAction :: MonadBaseControl IO m
               => m ()
               -> Delay
               -> m ThreadId
repeatedAction action delay = fork (forever $ suspend delay >> action)
{-# INLINE repeatedAction #-}
