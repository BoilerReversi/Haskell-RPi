-- TODO
-- Put this in a proper module and name it
-- Other useful features for the board (if there are any more?)

import Data.Word
import qualified Data.ByteString as B

-- 192 bits that are sent to the board
uartOutBytes :: Int
uartOutBytes = 24

data Color = Blank | Red | Green | Blue deriving (Enum, Show)

encode :: (Integral a) => Color -> a
encode Blank = 0
encode Red = 1
encode Green = 2
encode Blue = 3

decode :: (Integral a) => a -> Color
decode 0 = Blank
decode 1 = Red
decode 2 = Green
decode 3 = Blue

boardToBits :: (a -> Color) -> [a] -> B.ByteString
boardToBits f = B.pack . take uartOutBytes . (++ [0, 0 ..]) . map (encode . f)

-- UNTESTED ATM
bitsToBoard :: (Color -> a) -> B.ByteString -> [a]
bitsToBoard f = map (f . decode) . B.unpack