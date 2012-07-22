module Control.Concurrent.Timer.Types
( Timer(..)
, TimerImmutable(..)
) where

------------------------------------------------------------------------------
import           Control.Concurrent         (ThreadId)
import           Control.Concurrent.MVar    (MVar)
import           Control.Concurrent.Suspend (Delay)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | The data type representing the timer.
-- For now, the action and delay are fixed for the lifetime of the Timer.
data Timer m = Timer
    { timerImmutable :: MVar (Maybe (TimerImmutable m)) -- ^ If the MVar is empty, someone if mutating the timer. If the MVar contains Nothing, the timer was not started/initialized.
    }

data TimerImmutable m = TimerImmutable
    { timerAction   :: m ()
    , timerDelay    :: Delay
    , timerThreadID :: ThreadId
    }
