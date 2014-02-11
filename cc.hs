import System.Hardware.GPIO.Pin as P

import Control.Concurrent
import Control.Monad
import Control.Concurrent.Chan

main = let inputs = [newGpioEvent 7 g]
           outputs = [Immediate (print . head)]
           update msg state = tail state
       in  topLevel inputs update (cycle "GO PURDUE ") outputs

data Message = Undo | Reset | Legal Bool deriving (Show)

f Rising = Just Undo
f Falling = Nothing

g Rising = Just (Legal True)
g Falling = Just (Legal False)

-- BEGIN: OUR FRAMEWORK
-- Will eventually be in some module like
-- RPi.RaspberryLambda.something. Naming is difficult actually

-- I don't like this type synonym actually.
type InputAction m = IO m

data OutputAction s = Immediate (s -> IO ()) 
                    | Repeat Int (s -> IO ())

-- Rename this to something less dumb than topLevel
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
                       
-- ...
newInputHandler :: Chan m -> InputAction m -> IO ()
newInputHandler ch action = forever $ do msg <- action
                                         writeChan ch msg
                                         
-- Creates an OutputHandler IO () (to be forked presumably) that will
-- read state from MVar and output according to it. Immediate OutputActions
-- will occur as soon as state is updated*. Repeat OutputActions will
-- occur at a set interval regardless of updated state
newOutputHandler :: OutputAction s -> MVar s -> IO ()
newOutputHandler (Immediate output) mv = forever $ do state <- takeMVar mv
                                                      output state
newOutputHandler (Repeat interval output) mv =
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

-- Gpio listener wrapper. Will push the unwrapped result of 
-- Edge -> Maybe m if it returns Just. So you can use that function
-- to choose to fire messages on the rising and/or falling edges
-- of GPIO.
data Edge = Rising | Falling deriving (Show)
newGpioEvent :: Int -> (Edge -> Maybe m) -> InputAction m
newGpioEvent pinNumber newMsg  = do p <- P.init pinNumber In
                                    val <- P.read p
                                    loop p val
  where
    loop pin prev = do
      curr <- P.read pin
      let reaction = case (prev, curr) of (Zero, One) -> newMsg Rising
                                          (One, Zero) -> newMsg Falling
                                          (_, _) -> Nothing
      case reaction of (Just msg) -> do P.close pin
                                        return msg
                       Nothing -> loop pin curr

-- Periodic event. Will periodically input m to the Chan
newPeriodicEvent :: Int -> m -> InputAction m
newPeriodicEvent interval msg = do threadDelay interval
                                   return msg
