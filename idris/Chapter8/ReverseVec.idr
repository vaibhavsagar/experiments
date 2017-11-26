import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof $ myReverse xs ++ [x]
  where
    reverseProof : Vect (len + 1) elem -> Vect (S len) elem
    reverseProof {len} ys = rewrite plusCommutative 1 len in ys
