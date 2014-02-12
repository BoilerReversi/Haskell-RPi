import System.RaspberryPi.Events

-- This file is just a testbench for testing the library code

edgeToNumber :: Edge -> Int
edgeToNumber Rising = 100
edgeToNumber Falling = -7 

g Rising = Just 10
g Falling = Nothing

main = let inputs = [InputAction Immediate $ f g (getGpioEdge 7)]
           initial = 0
           update msg state = msg
           outputs = [OutputAction Immediate print]
       in topLevel inputs update initial outputs

-- This should probably be in Events.hs (or something more general even),
-- but I don't have a good name for it. It's a pretty common pattern though.
f :: (a -> Maybe b) -> IO a -> IO b
f verify action = do val <- action
                     let reaction = verify val
                     case reaction of (Just v) -> return v
                                      Nothing -> f verify action