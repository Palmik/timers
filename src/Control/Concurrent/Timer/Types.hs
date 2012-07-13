module Control.Concurrent.Timer.Types
( Timer(..)
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
    { timerAction   :: m ()
    , timerDelay    :: Delay
    , timerThreadID :: MVar (Maybe ThreadId)
    }
