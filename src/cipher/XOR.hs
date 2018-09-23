module XOR where

xorBool :: Bool -> Bool -> Bool
xorBool True True = False
xorBool True False = True
xorBool False True = True
xorBool False False = False

xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor = zipWith xorBool

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

-- max number of bits for Ints
maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = paddingFalses ++ reversedBits
  where
    reversedBits = reverse $ intToBits' n
    missingBits = maxBits - length reversedBits
    paddingFalses = replicate missingBits False

charToBits :: Char -> Bits
charToBits ch = intToBits $ fromEnum ch

bitsToInt :: Bits -> Int
bitsToInt [] = 0
bitsToInt xs = go 0 xs
  where
    go n [] = n
    go n (x:xs) = go (2 * n + f x) xs
    f True = 1
    f False = 0

bitsToChar :: Bits -> Char
bitsToChar xs = toEnum $ bitsToInt xs

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = zipWith xor padBits plainTextBits
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
  where
    bitList = applyOTP' pad plainText

myPad = "Shhhhhh"

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- A Cipher class to generalize cipher operations
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String
