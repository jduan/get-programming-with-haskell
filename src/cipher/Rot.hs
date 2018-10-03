module Cipher.Rot where

data FourLetterAlphabet
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

-- Rotate a letter to the right by half of the size of the alphabet
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabet
    rotation = offset `mod` alphabetSize

-- Decode a letter to its original. Handle odd-numbered alphabets.
rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset =
      if even n
        then fromEnum c + halfN
        else 1 + fromEnum c + halfN
    rotation = offset `mod` n

rotChar :: Char -> Char
rotChar ch = rotN sizeOfAlphabet ch
  where
    sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- Encode a message by calling rotN
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder msg = map f msg
  where
    f ch = rotN size ch
    size = 1 + fromEnum (maxBound :: FourLetterAlphabet)

message = [L1, L3, L4, L1, L1, L2]

rotEncoder :: String -> String
rotEncoder text = map f text
  where
    f ch = rotN size ch
    size = 1 + fromEnum (maxBound :: Char)

rotDecoder :: String -> String
rotDecoder text = map f text
  where
    f ch = rotNDecoder size ch
    size = 1 + fromEnum (maxBound :: Char)

mainCipher :: IO ()
mainCipher = do
  print $ rotDecoder (rotEncoder "hello world")
  print $ rotDecoder (rotEncoder "Jean-Paul likes Simone")
