module Board (
  Color(..),
  boardToByteString,
  boardToWords,
  boardToBools
  ) where

-- TODO
-- Put this in a proper module and name it
-- Other useful features for the board (if there are any more?)
-- Maybe call it grid or something? We should name our generic board.
--  Maybe something like BoilerBoard or BoilerGrid?

import Data.Word
import Data.Bits
import qualified Data.ByteString as B

-- Constants
uartOutBytes :: Int
uartOutBytes = 24

uartOutBits :: Int
uartOutBits = 8 * uartOutBytes

boardSize :: Int
boardSize = 64

-- Color type and its en/decoding
-- TODO: 
--  Use all colors we will be using.
--  Have the encoding match the bits to our LED driver for convenience
--    Document that we made that domain-specific decision for convenience
data Color = Blank | Red | Green | Blue deriving (Enum, Show)

encode :: Color -> Int
encode Blank = 0
encode Red = 1
encode Green = 2
encode Blue = 4

decode :: Int -> Color
decode 0 = Blank
decode 1 = Red
decode 2 = Green
decode 3 = Blue

-- Top level transformations of board representation
boardToByteString :: (a -> Color) -> [a] -> B.ByteString
boardToByteString f = B.pack . boardToWords f

-- 
boardToWords :: (a -> Color) -> [a] -> [Word8]
boardToWords f = map pack . partition 8 . boardToBools f

--
boardToBools :: (a -> Color) -> [a] -> [Bool]
boardToBools f = take uartOutBits . pad . foldr extractLower3 [] . (map (encode . f))
  where
  	pad = (++ [False, False ..])

-- Helpers
pack :: [Bool] -> Word8
pack = foldl (\acc x-> if x then (setBit (shiftL acc 1) 0) else (shiftL acc 1)) 0 . take 8

partition :: Int -> [a] -> [[a]]
partition n [] = []
partition n xs = (take n xs) : (partition n (drop n xs))

extractLower3 :: (Bits a) => a -> [Bool] -> [Bool]
extractLower3 x acc = (testBit x 0) : (testBit x 1) : (testBit x 2) : acc
