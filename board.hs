-- TODO
-- Put this in a proper module and name it
-- Other useful features for the board (if there are any more?)

import Data.Word
import Data.Bits
import qualified Data.ByteString as B

-- 192 bits that are sent to the board
uartOutBytes :: Int
uartOutBytes = 24

boardSize :: Int
boardSize = 64

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

-- EVERYTHING BELOW THIS IS PRETTY MESSED UP
-- THIS IS WRONG. It puts each color in a whole byte. We want it in 3 bits.
-- I'll have to fix this, but it'll be a pain due to multiples of 3 not
-- playing nice with [Word8]

boardToBits :: (a -> Color) -> [a] -> B.ByteString
boardToBits f = B.pack . take uartOutBytes . map pack3PerByte . chunk . take boardSize . (++ [0, 0 ..]) . map (encode . f)

boardToBitsOLD :: (a -> Color) -> [a] -> B.ByteString
boardToBitsOLD f = B.pack . take uartOutBytes . (++ [0, 0 ..]) . map (encode . f)

-- Truncates if length [a] is not a multiple of 3
chunk :: [a] -> [(a,a,a)]
chunk (x:y:z:xs) = (x,y,z) : (chunk (z:xs))
chunk _ = []

-- UNTESTED AND WRONG! ATM
bitsToBoard :: (Color -> a) -> B.ByteString -> [a]
bitsToBoard f = map (f . decode) . B.unpack

pack3PerByte :: (Word8, Word8, Word8) -> Word8
pack3PerByte (a,b,c) = (lower3 a) .|. 
                       (shiftL (lower3 b) 3) .|. 
                       (shiftL (lower3 c) 6)
  where
    lower3 v = v .&. 7
