module Cipher.Cipher where

import Cipher.Rot
import Cipher.XOR

-- A Cipher class to generalize cipher operations
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot =
  Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad =
  OTP String

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
