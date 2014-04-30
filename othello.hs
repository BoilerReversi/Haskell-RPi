import qualified System.Hardware.GPIO.Pin as P
import System.RaspberryPi.Events
import Game.Othello hiding (White)
import System.Hardware.Serialport
import qualified Data.ByteString as B
import Board
import Data.Word
import System.Process(readProcess)

data GameState = GameState { board :: [Board]
                           , ai :: Bool
                           , legal :: Bool } deriving (Show)

data Message = Undo | Reset | Mark (Int, Int)
             | Save | Load | Suggest Bool | Legal Bool deriving (Show)

-- TODO
-- Get inputs working (uart, buttons (legal, undo, ai, reset))
-- State just falls through
-- print most recent message

byteToCoord :: Int -> (Int, Int)
byteToCoord w = (w `div` 8, w `mod` 8)

uartListener :: SerialPort -> IO Message
uartListener s = do r <- recv s 1
                    case B.unpack r of
                      [] -> uartListener s
                      (w:_) 
                        | w < 64 -> return $ Mark $ byteToCoord $ fromIntegral w
                        | otherwise -> uartListener s

risingListener :: P.Pin -> m -> IO m
risingListener n msg = do e <- getGpioPinEdge n
                          case e of
                            Rising -> return msg
                            Falling -> risingListener n msg

edgeListener :: P.Pin -> (Bool -> m) -> IO m
edgeListener n f = do e <- getGpioPinEdge n
                      case e of 
                        Rising -> return $ f True
                        Falling -> return $ f False

squareToColor :: OutputSquare -> Color
squareToColor X = Red
squareToColor O = Blue
squareToColor E = White
squareToColor L = Green
squareToColor AI = Green

uartBoard s b = mapM_ (mysend s) $ boardToWords squareToColor b

mysend s w = send s $ B.pack [w]

outputGameState :: SerialPort -> GameState -> IO ()
outputGameState s (GameState b False False) = uartBoard s $ outputList False $ head b
outputGameState s g@(GameState b _ True) = uartBoard s $ outputList True $ head b
outputGameState s g@(GameState b True False) = do print "DOING AI"
                                                  x <- readProcess "python" ["client.py", toEdaxString (head b)] []
                                                  print x
doAI :: GameState -> IO ()
doAI g@(GameState b False _) = return ()
doAI g@(GameState b True _) = do print "DOING AI"
                                 x <- readProcess "python" ["client.py", toEdaxString (head b)] []
                                 print x

updateGameState :: Message -> GameState -> GameState
updateGameState (Mark c) g = if (isLegal (head $ board g) c)
                             then g { board = ((makeMove (head $ board g) c) : (board g)) }
                             else g
updateGameState Undo g = case (board g) of
  [x] -> g
  (x:xs) -> g {board = xs}
updateGameState Reset g = GameState {board = [initialBoard], ai = False, legal = False }
updateGameState (Suggest x) g = g { ai = x }
updateGameState (Legal x) g = g { legal = x }

main = do undoPin <- P.init 0 P.In
          resetPin <- P.init 1 P.In
          aiPin <- P.init 7 P.In
          legalPin <- P.init 3 P.In
          s <- openSerial "/dev/ttyAMA0" defaultSerialSettings { commSpeed = CS9600 }
          let inputs = map (InputAction Immediate) [uartListener s,
                                                    risingListener undoPin Undo,
                                                    risingListener resetPin Reset,
                                                    edgeListener aiPin Suggest,
                                                    edgeListener legalPin Legal]
              initial = GameState {board = [initialBoard], ai = False, legal = False }
              update = updateGameState
              outputs = [OutputAction Immediate $ outputGameState s]
          topLevel inputs update initial outputs
