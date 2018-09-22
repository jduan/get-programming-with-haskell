module XOR where

xorBool :: Bool -> Bool -> Bool
xorBool True True = False
xorBool True False = True
xorBool False True = True
xorBool False False = False
