import Data.Vect

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = 0 :: allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
