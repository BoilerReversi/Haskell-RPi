import System.Hardware.GPIO.Pin as P

import Control.Concurrent
import Control.Monad
import Control.Concurrent.Chan

main = let inputs = [newGpioEvent 7 g]
           outputs = [Immediate print]
           update msg state = msg
       in  topLevel inputs update Reset outputs

data Message = Undo | Reset | Legal Bool deriving(Show)

f Rising = Just Undo
f Falling = Nothing

g Rising = Just (Legal True)
g Falling = Just (Legal False)

-- BEGIN: OUR FRAMEWORK
data OutputAction s = Immediate (s -> IO ()) | Repeat Int (s -> IO ())

topLevel :: [(Chan m -> IO ())] ->
            (m -> s -> s) ->
            s ->
            [OutputAction s] ->
            IO ()
topLevel inputs update initial outputs = 
  do
    -- fork and listen to inputs
    ch <- newChan
    mapM_ forkIO $ map ($ ch) inputs
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


updateMVar :: MVar a -> a -> IO ()
updateMVar mv val = do tryTakeMVar mv
                       putMVar mv val

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

-- Helpers to plug into top-level
data Edge = Rising | Falling deriving(Show)

newGpioEvent :: Int -> (Edge -> Maybe m) -> Chan m -> IO ()
newGpioEvent pinNumber newMsg ch = do p <- P.init pinNumber In
                                      loop p Zero
  where
    loop pin prev = do
      curr <- P.read pin
      let reaction = case (prev, curr) of (Zero, One) -> newMsg Rising
                                          (One, Zero) -> newMsg Falling
                                          (_, _) -> Nothing
      case reaction of (Just msg) -> writeChan ch msg
                       Nothing -> return ()
      loop pin curr
