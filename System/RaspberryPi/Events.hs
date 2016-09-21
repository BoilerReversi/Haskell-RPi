module System.RaspberryPi.Events (
  InputAction(..),
  OutputAction(..),
  Timing(..),
  Edge(..),
  topLevel,
  getGpioEdge,
  getGpioPinEdge,
  newGpioEvent,
  newPeriodicEvent
  ) where

import qualified System.Hardware.GPIO.Pin as P

import Control.Concurrent
import Control.Monad
import Control.Concurrent.Chan

data Timing = Immediate | Repeat Int
data InputAction m = InputAction Timing (IO m)
data OutputAction s = OutputAction Timing (s -> IO ()) 

-- This function will...
-- Tie each InputAction to the same central Chan, to which each
--  InputAction will push messages of type m
-- Fork on each InputAction to listen to each event concurrently
-- Pull messages out of the central Chan and update state according to
--  (m -> s -> s)
-- Push updated state to concurrent output handlers, which will produce
-- output according to the provided OutputAction
topLevel :: [InputAction m] ->
            (m -> s -> s) ->
            s ->
            [OutputAction s] ->
            IO ()
topLevel inputs update initial outputs = 
  do
    -- fork and listen to inputs
    ch <- newChan
    mapM_ forkIO $ map (newInputHandler ch) inputs
    -- fork and wait on outputs
    -- outStates is a list of MVar s
    outStates <- mapM newMVar $ replicate (length outputs) initial
    mapM_ forkIO $ zipWith newOutputHandler outputs outStates
    loop ch outStates initial
  where
    loop ch outStates state = do msg <- readChan ch
                                 let newState = update msg state
                                 mapM_ (`updateMVar` newState) outStates
                                 loop ch outStates newState


-- Changes the MVar's contents to the inputted value, whether or
-- not the MVar is full. Only works as expected if the MVar has
-- ONE and only ONE producer
updateMVar :: MVar a -> a -> IO ()
updateMVar mv val = do tryTakeMVar mv
                       putMVar mv val
                       
newInputHandler :: Chan m -> InputAction m -> IO ()
newInputHandler ch (InputAction Immediate action) = forever $ do msg <- action
                                                                 writeChan ch msg
newInputHandler ch (InputAction (Repeat interval) action) = forever $ do msg <- action
                                                                         writeChan ch msg
                                                                         threadDelay interval

newOutputHandler :: OutputAction s -> MVar s -> IO ()
newOutputHandler (OutputAction Immediate output) mv = forever $ do state <- takeMVar mv
                                                                   output state
newOutputHandler (OutputAction (Repeat interval) output) mv =
  do state <- takeMVar mv
     output state
     loop state
  where
    loop state = do poll <- tryTakeMVar mv
                    let nextState = case poll of (Just s) -> s
                                                 Nothing -> state
                    output nextState
                    threadDelay interval
                    loop nextState

-- Input event helpers

data Edge = Rising | Falling deriving (Show)

getGpioPinEdge :: P.Pin -> IO Edge
getGpioPinEdge p = do val <- P.read p
                      loop p val
  where
    loop pin prev = do
      curr <- P.read pin
      case (prev, curr) of (Zero, One) -> return Rising
                           (One, Zero) -> return Falling
                           (_, _) -> loop pin curr
                           
getGpioEdge :: Int -> IO Edge
getGpioEdge pinNumber = do p <- P.init pinNumber In
                           val <- P.read p
                           loop p val
  where
    loop pin prev = do
      curr <- P.read pin
      case (prev, curr) of (Zero, One) -> do P.close pin
                                             return Rising
                           (One, Zero) -> do P.close pin
                                             return Falling
                           (_, _) -> loop pin curr
                                      
newGpioEvent :: Int -> (Edge -> Maybe m) -> InputAction m
newGpioEvent pinNumber newMsg = InputAction Immediate getMsg
  where
    getMsg = do e <- getGpioEdge pinNumber
                let reaction = newMsg e
                case reaction of (Just msg) -> return msg
                                 Nothing -> getMsg
                                   
-- Periodic event.
newPeriodicEvent :: Int -> m -> IO m
newPeriodicEvent interval msg = do threadDelay interval
                                   return msg

