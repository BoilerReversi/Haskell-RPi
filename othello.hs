import qualified System.Hardware.GPIO.Pin as P
import System.RaspberryPi.Events
import Game.Othello hiding (White)
import System.Hardware.Serialport
import qualified Data.ByteString as B
import Board
import Data.Word
import System.Process(readProcess)
import System.Directory (doesFileExist)
savePath :: String
savePath = "OTHELLOSAVE"

data GameState = GameState { board :: [Board]
                           , ai :: Bool
                           , legal :: Bool 
                           , save :: Bool} deriving (Show)

data Message = Undo | Reset | Mark (Int, Int)
             | Save | Load Board | Suggest Bool | Legal Bool deriving (Show)

byteToCoord :: Int -> (Int, Int)
byteToCoord w = (w `div` 8, w `mod` 8)

uartListener :: SerialPort -> IO Message
uartListener s = do r <- recv s 1
                    case B.unpack r of
                      [] -> uartListener s
                      (w:_) 
                        | w < 64 -> return $ Mark $ byteToCoord $ fromIntegral w
                        | w == 75 -> do e <- doesFileExist savePath
                                        if e
                                          then do print "Loading `OTHELLOSAVE`"
                                                  x <- readFile savePath
                                                  print $ "LOADED " ++ x
                                                  return $ Load $ fromEdaxString x
                                          else uartListener s

                        | w == 95 -> do print "got 95!"
                                        return Save
                        | otherwise -> uartListener s

saveAction :: GameState -> IO ()
saveAction (GameState (b:_) _ _ True) = writeFile savePath $ toEdaxString b
saveAction (GameState _ _ _ False) = return ()

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

outputGameState :: SerialPort -> String -> GameState -> IO ()
outputGameState s _ (GameState b False False _) = uartBoard s $ outputList False False $ head b
outputGameState s _ g@(GameState b _ True _) = uartBoard s $ outputList True False $ head b
outputGameState s ip g@(GameState b True False _) = do print "DOING AI"
                                                       x <- readProcess "python" ["client.py", ip, toEdaxString (head b)] []
                                                       let c = j x
                                                           ls = legalMoves (head b)
                                                       if (isLegal (head b) c)
                                                         then uartBoard s $ replaceAt (h x) (outputList False False $ head b) AI
                                                         else uartBoard s $ outputList False True $ (head b)
                                                       print x
                         
  where
    f 'A' = 0
    f 'B' = 1
    f 'C' = 2
    f 'D' = 3
    f 'E' = 4
    f 'F' = 5
    f 'G' = 6
    f 'H' = 7
    f _ = 7
    
    g '1' = 0
    g '2' = 1
    g '3' = 2
    g '4' = 3
    g '5' = 4
    g '6' = 5
    g '7' = 6
    g '8' = 7
    g _ = 7
    
    h (x:y:_) = ((f x) * 8) + (g y)
    j (x:y:_) = (f x, g y)
    k (x, y) = (x * 8) + y
    replaceAt n xs v = take n xs ++ [v] ++ drop (n + 1) xs

doAI :: GameState -> IO ()
doAI g@(GameState b False _ _) = return ()
doAI g@(GameState b True _ _) = do print "DOING AI"
                                   x <- readProcess "python" ["/root/Haskell-RPi/client.py", toEdaxString (head b)] []
                                   print x

updateGameState :: Message -> GameState -> GameState
updateGameState (Mark c) g = if (isLegal (head $ board g) c)
                             then g { board = ((makeMove (head $ board g) c) : (board g)), save = False }
                             else g { save = False }
updateGameState Undo g = case (board g) of
  [x] -> g {save = False}
  (x:xs) -> g {board = xs, save = False}
updateGameState Reset g = GameState {board = [initialBoard], ai = False, legal = False , save = False}
updateGameState (Suggest x) g = g { ai = x , save = False}
updateGameState (Legal x) g = g { legal = x , save = False}
updateGameState Save g = g {save = True}
updateGameState (Load b) g = g {board = [b], save = False}

main = do putStrLn "Enter server IP:"
          ip <- getLine
          undoPin <- P.init 0 P.In
          resetPin <- P.init 1 P.In
          aiPin <- P.init 7 P.In
          legalPin <- P.init 3 P.In
          s <- openSerial "/dev/ttyAMA0" defaultSerialSettings { commSpeed = CS9600 }
          let inputs = map (InputAction Immediate) [uartListener s,
                                                    risingListener undoPin Undo,
                                                    risingListener resetPin Reset,
                                                    edgeListener aiPin Suggest,
                                                    edgeListener legalPin Legal]
              initial = GameState {board = [initialBoard], ai = False, legal = False, save = False }
              update = updateGameState
              outputs = [OutputAction Immediate $ (printBoard . head . board),
                         OutputAction Immediate $ outputGameState s ip,
                         OutputAction Immediate $ saveAction]
          topLevel inputs update initial outputs
